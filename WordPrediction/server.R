# Word Prediction
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(qdap)
library(stringr)
library(stringi)
library(knitr)   # combine_words
library(quanteda)  # char_tolower

#mydt<-readRDS("~/coursera/DS_Capstone/rds/tc56.ngrams.rds")
#mydt<-readRDS("~/coursera/DS_Capstone/rds/trainc16.ngrams16.nolookup.freq3.ngrams.rds")
#mydt<-readRDS("~/coursera/DS_Capstone/rds/ngrams16hct.all_nolookup.freq20.ngrams.rds")
#mydt<-readRDS("~/coursera/DS_Capstone/rds/ngrams16hct.all_nolookup.freq10.ngrams.rds")
#mydt<-readRDS("~/coursera/DS_Capstone/rds/ngrams16hct.all_nolookup.freq6.ngrams.rds")
mydt<-readRDS("dscapstone.ngrams.rds")

myPredict2 <- function(d, str) {
  if(is.na(wc(str))) return(c("Hi", "I", "Thanks"))
  # if(str=="") return(c("Hi", "I", "Thanks"))
  # d<- mydt
  
  # str <- char_tolower(sub("\\s+$", "", str))
  str <- char_tolower(str)
  n <- wc(str)
  max_ngram <- max(d$n)
  # get last max_ngram words from str
  last_wc <- ifelse(max_ngram <= n, max_ngram-1, n)
  # consider only prior max_ngram words
  # str <- combine_words(word(str, -last_wc:-1), and = " ", sep = " ")
  
  # str<-"about the censor wh"
  # build patterns for each ngram - about the censor which
  # 1. ^wh
  # 2. ^censor wh
  # 3. ^the censor wh
  # 4. ^about the censor wh
  # d<-mydt
  d1<-as.data.table(NULL)
  for (i in wc(str):1) {
    patn<-paste0("^", combine_words(word(str, -i:-1), and = " ", sep = " "), "[a-z]*$")
    # print(patn)
    
    if(i==1) {
      # d1<-rbind(d1,copy(head(d[grep(patn, ngrams)][order(-freq)], 3)))
      d1<-rbind(d1,copy(head(d[stri_detect_regex(ngrams, patn)][order(-freq)], 3)))
    } else {
      d1<-rbind(d1, copy(head(d[stri_detect_regex(ngrams, patn)][order(-prob)],3)))
    }
  } 
  
  # cleanup
  # filter out matching 1gram
  # remove duplecate pred words
  #> head(mydt[grep("^is the ma[a-z]*$", ngrams)][order(-prob)], 3)
  #ngrams freq n lookup pred         prob
  #1: is the main    4 3 is the main 0.0001657138
  #> head(mydt[grep("^the ma[a-z]*$", ngrams)][order(-prob)], 3)
  #ngrams freq n lookup   pred        prob
  #1:   the main   81 2    the   main 0.003355705
  #2:    the man   77 2    the    man 0.003189991
  #3: the market   57 2    the market 0.002361422
  #> head(mydt[grep("^ma[a-z]*$", ngrams)][order(-prob)], 3)
  #ngrams freq n lookup   pred prob
  #1:     ma   10 1            ma    1
  #2:   maas    3 1          maas    1
  #3: mabley    8 1        mabley    1
  
  # mypred <- d1[,.(pred,prob), by=c("pred","prob")][,pred][1:3]
  mypred <- d1[ngrams!=word(str, wc(str)),.(pred,prob), by=c("pred","prob")][,unique(pred)][1:3]
  ifelse(sum(is.na(mypred))==0, mypred,  mypred[is.na(mypred)]<-c("I","we","is")[1:sum(is.na(mypred))])
    
  #if (substr(str, nchar(str),nchar(str))==" ") {
  #  str <- sub("\\s+$", "", str)
  #  patn <- paste0(".*", str, "[a-z]+$")
  #} else {
  #  patn <- paste0(".*", str, ".*")
  #}
  
  mypred
}


myPredict <- function(d, str) {
  
  str <- char_tolower(sub("\\s+$", "", str))
  n <- wc(str)
  max_ngram <- max(d$n)
  
  # get last max_ngram words from str
  last_wc <- ifelse(max_ngram <= n, max_ngram-1, n)
  str <- combine_words(word(str, -last_wc:-1), and = "", sep = " ")
  str <- sub("\\s+$", "", str)
  patn <- paste0("^", str, "$")
  #print(paste0("my patn : ", patn, " ", last_wc), quote = FALSE)
  mypred<-d[grep(patn, d$lookup),.(pred, prob)][order(-prob)][,pred][1:3]
  ifelse(sum(is.na(mypred))==0, mypred,  mypred[is.na(mypred)]<-c("I","we","is")[1:sum(is.na(mypred))])
  mypred
}


shinyServer(function(session, input, output) {
  
  observeEvent(input$pred1, {
    # updateTextInput(session, "text", value=paste0(sub("\\s+$", "", input$text), mypred[1]))
    if(is.na(wc(input$text))) {
      updateTextInput(session, "text", value=paste0(mypred[1], " "))
    } else {
        if (substr(input$text, nchar(input$text),nchar(input$text))==" ")
            updateTextInput(session, "text", value=paste0(combine_words(word(input$text, 1:wc(input$text)), and = " ", sep = " "), " ", mypred[1]))
        else
            updateTextInput(session, "text", value=paste0(combine_words(word(input$text, 1:(wc(input$text)-1)), and = " ", sep = " "), " ", mypred[1]))
    }
    
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
  observeEvent(input$pred2, {
    if(is.na(wc(input$text))) {
      updateTextInput(session, "text", value=paste0(mypred[2], " "))
    } else {
        if (substr(input$text, nchar(input$text),nchar(input$text))==" ")
            updateTextInput(session, "text", value=paste0(combine_words(word(input$text, 1:wc(input$text)), and = " ", sep = " "), " ", mypred[2]))
        else
            updateTextInput(session, "text", value=paste0(combine_words(word(input$text, 1:(wc(input$text)-1)), and = " ", sep = " "), " ", mypred[2]))
    }
    
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })

  observeEvent(input$pred3, {
    if(is.na(wc(input$text))) {
      updateTextInput(session, "text", value=paste0(mypred[3], 0))
    } else {
        if (substr(input$text, nchar(input$text),nchar(input$text))==" ")
            updateTextInput(session, "text", value=paste0(combine_words(word(input$text, 1:wc(input$text)), and = " ", sep = " "), " ", mypred[3]))
        else
            updateTextInput(session, "text", value=paste0(combine_words(word(input$text, 1:(wc(input$text)-1)), and = " ", sep = " "), " ", mypred[3]))
    }
    
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  

  output$message <- renderText({
    # Simply accessing input$goButton here makes this reactive
    # object take a dependency on it. That means when
    # input$goButton changes, this code will re-execute.
    #input$goButton
  
    #if(str_detect(input$text, ".*\\s$")) {
        mypred <<-myPredict2(mydt, input$text)
        updateActionButton(session, "pred1", label = mypred[1])
        updateActionButton(session, "pred2", label = mypred[2])
        updateActionButton(session, "pred3", label = mypred[3])
    #}

    # input$text is accessed here, so this reactive object will
    # take a dependency on it. However, input$ is inside of
    # isolate(), so this reactive object will NOT take a
    # dependency on it; changes to input$n will therefore not
    # trigger re-execution.

    paste0(input$text)
  })
  
})

