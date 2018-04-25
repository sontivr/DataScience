#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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
library(httr)

# request <- GET(url="https://dl.dropboxusercontent.com/s/rb0cnyfiz2fgdaw/hello.html")
#request <- GET(url="DJIA_DailyClosingPricesDoc.html")
#user_doc.html <-content(request, as="text")
user_doc.html <- "DJIA_DailyClosingPricesDoc.html"

# Define server logic required to draw the trend chart for the year selected by the user
shinyServer(function(input, output, session) {
  
  observeEvent(input$slider_year,{
    updateSelectInput(session, "select_year", selected = input$slider_year)
  })
  
  #observeEvent(input$show_doc, {
  #  output$showdoc <- renderUI({user_doc.html})
    
  #})
  
  observeEvent(input$select_year, {   
    output$myplot <-  renderPlotly({
       getSymbols('^DJI',src='yahoo', from=paste0(as.character(input$select_year), '-01-01'), to=paste0(as.character(input$select_year), '-12-31'))
    
       output$textout <- renderText(paste0("Server getting data for the year ", input$select_year, ": ", "getSymbols('^DJI',src='yahoo', from='", as.character(input$select_year), "-01-01', to='", paste0(as.character(input$select_year), "-12-31')")))
    
       trends <- data.frame(Date = index(DJI), coredata(DJI)) %>% select(Date, DJI.Close)
       hovertext1 <- paste0("Date:<b>", trends$Date, "</b><br>",
                         "DJI.Close:<b>", trends$DJI.Close, "</b><br>")
    
       updateSliderInput(session, "slider_year", value = input$select_year)
       plot_ly(data = trends, x = ~Date) %>%
               add_lines(y = ~DJI.Close, line = list(color = "#800000", width = 2),
                hoverinfo = "text", text = hovertext1, name = "DJI.Close")
    })
  })
})