#
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

# get the data from yahoo for initial population
getSymbols('^DJI',src='yahoo', from='2002-01-01', to='2002-12-31')

trends <- data.frame(Date = index(DJI), coredata(DJI)) %>% select(Date, DJI.Close)

hovertext1 <- paste0("Date:<b>", trends$Date, "</b><br>",
                     "DJI.Close:<b>", trends$DJI.Close, "</b><br>")

choice_years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)   

user_doc <- "DJIA Daily Closing Prices End User Document.html"


# Define UI for application that draws yearly trend chart
shinyUI(fluidPage(
  titlePanel("DJIA Daily Closing Prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider_year", label = "Years:", min = 2001, max = 2016, value = 1, step = 1, sep=""),
      selectInput("select_year", label = "Select Year:", choices = choice_years, selected = 2001)
      #,actionButton("show_doc", "Show Doc")
      #,includeHTML(user_doc)
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("plot", value = 1,
        # textOutput("textout"),
      plotlyOutput("myplot"))
      ,tabPanel("doc", value = 2, textOutput("textout")) #includeHTML(user_doc))
      )
      
    )
  #)
 )
  
 # fluidRow(includeHTML(user_doc))
)
#,fluidRow(column(12, includeHTML(user_doc)))

)
