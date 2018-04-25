# Word Prediction
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(data.table)
library(quantmod)
library(plotly)
library(zoo)
library(shiny)
#library(shinyjs)



shinyUI(fluidPage(align="center",
  tags$head(tags$script(
    'Shiny.addCustomMessageHandler("refocus",
    function(NULL) {
    document.getElementById("text").focus();
    });'
    )),
  titlePanel(em("TextSmart", style="color:#FF00FF")),
  p(em("Word Prediction App")),
  fluidRow(
    column(12, wellPanel(align="center",
      #sliderInput("n", "n (isolated):",
      #            min = 10, max = 1000, value = 200, step = 10),
      
      textInput("text", em("Start typing your message", style="color:red"), "", width = "80%"),
 
      br(),
      actionButton("pred1", "I"),
      actionButton("pred2", "Hi"),
      actionButton("pred3", "Thanks")
    )),
    column(12, align="center",
           # h4("Message"),
           textOutput("message"),
           tags$head(tags$style("#message{color: red;
                                 font-size: 16px;
                                font-style: italic;
                                }"
                         ))
    )
    #, tags$head(tags$script(src = "message-handler.js"))
    #, tags$textarea(id = "text_input1", rows =  2, cols = 50, "", autofocus = "autofocus")
  )
))
