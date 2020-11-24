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

source("Ban400-Functions.R")

risk_free_rate <- 0.02

tickersList <- stockSymbols()

tickers <- c("AAPL", "XOM", "BAC", "PFE", "NEE", "RTX")
from_date <- "2018-08-01"
to_date <- "2020-08-01"


functions_input <- stock_input(tickersList$Symbol[1:100],from_date ,to_date)
functions_input

# functions_input 1 = tickers
# functions_input 2 = stock_prices
# functions_input 3 = returns_matrix
# functions_input 4 = stock_correlation
# functions_input 5 = stock_return with date
# functions_input 6 = stock_covariance
# functions_input 7 = portfolio weigths

#finds the portfolio with the higest sharpe ratio
opt_sharpe <- stock_opt_sharpe(functions_input[[1]],functions_input[[7]],functions_input[[3]],functions_input[[6]])
opt_sharpe[[1]]

correlation_plot(functions_input[[4]])


ui <- fluidPage(
  headerPanel('Stockify - a stock portofolio optimizing app'),
  sidebarPanel(
    numericInput("rfrate", "Risk free rate: ", 0.03,
                 min = 0, 
                 max = 1,
                 step = 0.001)),
  mainPanel(
    plotOutput(inputId = "vcorr_plot"))
)

server <- function(input, output) {
  
  dataInput <- reactive({
    risk_free_rate <- input$rfrate
  })
  
  output$vcorr_plot <- renderPlot({
    correlation_plot(functions_input[[4]])})
  
}  # Make the server functions

shinyApp(ui = ui, server = server) # Combine it into the app





