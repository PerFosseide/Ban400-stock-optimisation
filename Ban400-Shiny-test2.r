library(shiny)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(corrplot)
library(nloptr)
library(gtools)
library(skimr)
library(svDialogs)
library(TTR)
library(markdown)
library(shinythemes)
#library(shinydashboard)

source("Ban400-Functions.R")

#risk_free_rate <- 0.03

tickersList <- stockSymbols()

#tickers <- c("AAPL", "XOM", "BAC", "PFE", "NEE", "RTX")

# Setting a default date
from_date <- "2018-08-01"
to_date <- "2020-08-01"



ui <- fluidPage(theme = shinytheme("cosmo"),
                
  navbarPage("Stockify",
             tabPanel("Home"),
             tabPanel("Code")),
  
#  includeCSS("Ban400-theme.css"),
  

  
  headerPanel('Find your optimal portofolio'),
  sidebarPanel(
    numericInput("rfrate", "Risk free rate: ", 0.03,
                 min = 0, 
                 max = 1,
                 step = 0.001),
    checkboxGroupInput("stockcategory", "Choose unfit categories:",
                       choiceNames = 
                         list("Alcohol", "Weapons", "Defense", "Crime", "Gambling", "Marijuana", "Tobacco"),
                       choiceValues = 
                         list("Alcohol", "Weapons", "Defense", "Crime", "Gambling", "Marijuana", "Tobacco")
                         ),
    dateInput("fromdate", "Date From: ", 
              from_date,
              min = "2007-08-01",
              max = "2020-10-01",
              format = "yyyy/mm/dd"
    ),
    dateInput("todate", "Date To: ",
              to_date,
              min = "2007-08-02",
              max = "2020-10-01",
              format = "yyyy/mm/dd"
    ),
    actionButton("update", "Update")),
  mainPanel(
    # -- Showing the outputs -- #
    
    # Stock Correlation 
    h4("Stock Correlation"),
    plotOutput("vcorr_plot"),
    
    # Sharpe ratio
#    h4("Sharpe Ratio"),
#    verbatimTextOutput("vopt_sharpe"),
    
    # Volatility
#    h4("Optimal Volatility"),
#    verbatimTextOutput("vopt_vol"),
    
    # Returns histogram
    h4("Returns Histogram"),
    plotOutput("vreturns_hist"),
    
    # Stock price history
    h4("Stock Price History"),
    plotOutput("vstock_price_history"),
    
    # Efficiency frontier
    h4("Efficiency Frontier"),
    plotOutput("vefficency_frontier")
    
    # Comparison with S&P500
#    h4("S&P500 Comparison"),
#    plotOutput("vcompare_SP500")
    )
)

server <- function(input, output) {
  
  
  ##############################################################################
  
  # Update stock_input with input from dateInput "fromdate" og "todate"
  # eventReactive means that it updates the function inputs every time user clicks the "update" button
  
  risk_free_rate <- eventReactive(input$update, {
    input$rfrate
  }, ignoreNULL = FALSE)
  
  
  dataInput <- eventReactive(input$update, {
    stock_input(tickersList$Symbol[1:10], input$fromdate, input$todate)
  }, ignoreNULL = FALSE)
  
  ##############################################################################
  
  # Generate output for the correlation plot
  output$vcorr_plot <- renderPlot({
    correlation_plot(dataInput()[[4]])
  })
  
  # Generate output for the sharpe ratio
#  output$vopt_sharpe <- renderText({
#    stock_opt_sharpe(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]])
#    stock_opt_sharpe[[1]] # does not work
#  })
  
  # Generate output for the volatility
#  volatilityInput <- reactive({
#    stock_opt_vol(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]])
#  })
  
#  output$vopt_vol <- renderTable({
#    volatilityInput[[1]]
#  })
  
  # Generate output for the returns histogram
  output$vreturns_hist <- renderPlot({
    returns_hist(dataInput()[[5]])
  })
  
  # Genereate output for the stock price history
  output$vstock_price_history <- renderPlot({
    stock_price_history(dataInput()[[2]])
  })
  
  # Generate output for the efficiency frontier
  output$vefficency_frontier <- renderPlot({
    efficency_frontier(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]], n = 5000)
  })
  
#  output$vcompare_SP500 <- renderPlot({
#    compare_SP500(as.matrix(vopt_vol[[2]]), dataInput()[[3]], input$fromdate, input$todate)
#  })
  
}
shinyApp(ui = ui, server = server) # Combine it into the app

# NB! Tar 1-2 min å laste




