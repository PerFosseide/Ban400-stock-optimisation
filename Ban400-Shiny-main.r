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
library(shinycssloaders)
library(RColorBrewer)



# Avoiding the navbar to be placed over the headerpanel
css <- HTML(" body{
        padding-top: 65px;
  }")


# setting a global loading icon color
options(spinner.color="#000000")

source("Ban400-Functions.R")



# Setting a default risk free rate
risk_free_rate <- 0.03

#tickersList <- stockSymbols()

#tickers <- c("AAPL", "XOM", "BAC", "PFE", "NEE", "RTX")
tickers <- sample(stocks_with_industry$Symbol, 50)
# Setting a default date
from_date <- "2018-08-01"
to_date <- "2020-08-01"


ui <- fluidPage(
  tags$main(tags$style(css)), # This argument will be applied in addition to the the theme
  theme = shinytheme("cosmo"),
                
                navbarPage("Portofolio optimizer", 
                           tabPanel("Stock Selection",
                                    
                                    headerPanel('Find your optimal portofolio'),
                                    sidebarPanel(
                                      numericInput("rfrate", "Risk free rate: ", risk_free_rate,
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
                                       
                                       # Optimal portofolio
                                       h3("Your optimal portofolio"),
                                       shinycssloaders::withSpinner(dataTableOutput("vopt_vol")))),
                                       
                           tabPanel("Optimalization Method",
                                    headerPanel("Choose preffered optimalization method"),
                                    sidebarPanel(
                                      selectInput("method", "Optimalization method:",
                                                  c("Sharpe Ratio Maximizing", 
                                                    "Volatility Minimizing", 
                                                    "Sortino Ratio Maximizing")), 
                                      mainPanel(
                                        h3("About the methods:"),
                                        h4("Sharpe ratio: "),
                                        h4("Volatility: "),
                                        h4("Sortino: "))
                                      )
                                  ),
                           
                           
                           tabPanel("Results",
                                    headerPanel("Your optimal portofolio"),
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Portofolio stats",
                                                 
                                                 # Portfolio stats
                                                 h3("Volatility min stats"),
                                                 shinycssloaders::withSpinner(tableOutput("vopt_stat")), # This needs to be dynamic and show the stats from the chosen method
                                                 
                                                 # Portfolio stats
                                                 h3("Sharpe max stats"),
                                                 shinycssloaders::withSpinner(tableOutput("vsharpe_stat")),
                                                 
                                                 # portfolio stats
                                                 h3("Stortino max stats"),
                                                 shinycssloaders::withSpinner(tableOutput("vsortino_stat")),
                                                 
                                                 #sortino
                                                 h4("Sortino ratio"), 
                                                 shinycssloaders::withSpinner(dataTableOutput("vsortino")),
                                                 
                                                 
                                                  ),
                                        tabPanel("Charts",
                                                 
                                                 # Stock Correlation 
                                                 h4("Portfolio"),
                                                 shinycssloaders::withSpinner(plotOutput("vpiechart")),
                                                 
                                                 # Stock Correlation 
                                                 h4("Stock Correlation"),
                                                 shinycssloaders::withSpinner(plotOutput("vcorr_plot")),
                                                 
                                                 # Returns histogram
                                                 h4("Returns Histogram"),
                                                 shinycssloaders::withSpinner(plotOutput("vreturns_hist")),
                                                 
                                                 # Stock price history
                                                 h4("Stock Price History"),
                                                 shinycssloaders::withSpinner(plotOutput("vstock_price_history")), 
                                                 
                                                 # Efficiency frontier
                                                 h4("Efficiency Frontier"),
                                                 shinycssloaders::withSpinner(plotOutput("vefficency_frontier")),
                                                 
                                                 # Comparison with S&P500
                                                 h4("S&P500 Comparison"),
                                                 shinycssloaders::withSpinner(plotOutput("vcompare_SP500"))
                                                 ))
                                      
                                      
                                    # portfolio stats
                      
                                    
                                    
                                    
                                    

                                    
                                    
                                   
                           ) # main panel
              ) # tab panel
              
    , position = "fixed-top") 
    # navbarPage
    
) # UI
                
  
                
server <- function(input, output) {
  
  
  
  ############################### PROCESSING INPUTS ###############################################
  
  # Update stock_input with input from dateInput "fromdate" og "todate"
  # eventReactive means that it updates the function inputs every time user clicks the "update" button
  
  risk_free_rate <- eventReactive(input$update, {
    input$rfrate
  }, ignoreNULL = FALSE)
  
  
  
  # -- Making a general function for user inputs to be applied to stock_input
  
  dataInput <- eventReactive(input$update, {
    input_function(tickers, input$fromdate, input$todate)
  }, ignoreNULL = FALSE)
  
  
  
  # -- Making another input function to be able to use subsets from the stock_opt_vol and stock_opt_sharpe functions.
  
  vol_output <- eventReactive(input$update, {
    stock_opt_vol(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]])
  }, ignoreNULL = FALSE)
  
  sharpe_output <- eventReactive(input$update, {
    stock_opt_sharpe(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]])
  }, ignoreNULL = FALSE)
  
  sortino_output <- eventReactive(input$update, {
    stock_opt_sortino(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]])
  }, ignoreNULL = FALSE)
  
  ############ END OF INPUT PROCESSING ##########
  
  
  
  
  ########### GENERATING OUTPUTS #############
  
  # Generate output for optimal volume
  output$vopt_vol <-renderDataTable({
    vol_output()[[3]] # Here we output a subset of vol_input
  })
  
  # Generate output for optimal volume
  output$vopt_stat <-renderTable({
    vol_output()[[4]] # Here we output a subset of vol_input
  })
  
  # Generate output for sharpe ratio
  output$vsharpe <-  renderDataTable({
    sharpe_output()[[3]]
  })
  # Generate output for optimal volume
  output$vsharpe_stat <-renderTable({
    sharpe_output()[[4]] # Here we output a subset of vol_input
  })
  
  
  # Generate output for sortino ratio
  output$vsortino <- renderDataTable({
    sortino_output()[[3]]
  })
  
  # Generate output for sortino ratio
  output$vsortino_stat <- renderTable({
    sortino_output()[[4]]
  })
  
  # Generate output for portfolio industry percentages
  output$vpiechart <- renderPlot({
    portfolio_industry(dataInput()[[1]], (sharpe_output()[[2]]))
  })
  
  
  # Generate output for the correlation plot
  output$vcorr_plot <- renderPlot({
    correlation_plot(dataInput()[[4]])
  })
  
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
  
  # Generate output for the S&P500 comparison
  # -- Denne virker ikke etter oppdateringen av funksjonslisten
  output$vcompare_SP500 <- renderPlot({
    compare_SP500(as.matrix(sharpe_output()[[2]]), dataInput()[[1]],dataInput()[[3]], input$fromdate, input$todate)
  })
  
  
}

shinyApp(ui = ui, server = server) # Combine it into the app

# Depending on the amount of stocks there can be some loading time (from 20 sec to 3 minutes)


