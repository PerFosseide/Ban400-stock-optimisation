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
library(stringr)



# Avoiding the navbar to be placed over the headerpanel
css <- HTML(" body{
        padding-top: 65px;
  }")

# Adding shadow to the navbar
css2 <- HTML(" nav {
  box-shadow: 0 5px 5px 0 rgba(0,0,0,.2);
}")



# setting a global loading icon color
options(spinner.color="#000000")

source("Ban400-Functions-old.R")



# Setting a default risk free rate
#risk_free_rate <- 0.03

#tickersList <- stockSymbols()

#tickers <- c("AAPL", "XOM", "BAC", "PFE", "NEE", "RTX")
#tickers <- sample_function(10)
# Setting a default date
from_date <- "2018-08-01"
to_date <- "2020-08-01"

# Setting a default max value for stock selection
max_stockselection <- 100


ui <- fluidPage(
  tags$main(tags$style(css)),
  tags$header(tags$style(css2)),
  theme = shinytheme("cosmo"),
  
  list(tags$head(HTML('<link rel="icon", href="Optimizer-logo2.png", 
                                   type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="Portfolio Optimizer"
      )
  ),
  
  
  navbarPage(title=div(img(src="Optimizer-logo2.png"), "Portfolio Optimizer"), id = "tabset1",
             
             tabPanel("Stock Selection",
                      
                      headerPanel('Find your optimal portofolio'),
                      sidebarPanel(
                        
                        numericInput("n_unique_stocks", "Amount of stocks to select a portfolio from: ", 10,
                                     min = 1, 
                                     max = max_stockselection,
                                     step = 1),
                        
                        selectInput("sinstock", "Avoid sinstocks?", c("Yes", "No"), "Yes"),
                      
                        
                        selectInput("greenonly", "Green Stocks Only?", c("Yes", "No"), "No"),
                        
                        selectInput("industry", "Select unfit industries:", stocks_with_industry$Industry, multiple = TRUE, selectize = TRUE
                        ),
                        
                        actionButton("random", "Get portfolio"), # Goal: Refresh a random selection of stocks within the selected categories
                        selectInput("manual", "Select stocks", stocks_with_industry$Symbol, multiple = TRUE, selectize = TRUE),
                        
                        numericInput("rfrate", "Risk free rate: ", 0.02,
                                     min = 0, 
                                     max = 1,
                                     step = 0.001),
                        
                        
                        dateInput("fromdate", "Test-data from: ", 
                                  from_date,
                                  min = "2007-08-01",
                                  max = "2020-10-01",
                                  format = "yyyy/mm/dd"
                        ),
                        dateInput("todate", "Test-data to: ",
                                  to_date,
                                  min = "2007-08-02",
                                  max = "2020-10-01",
                                  format = "yyyy/mm/dd"
                        ),
                        actionButton("update", "Confirm Selection")),
                      
                      mainPanel(
                        # -- Showing the outputs -- #
                        
                        # Optimal portofolio
                        h3("Available Stocks"),
                        shinycssloaders::withSpinner(dataTableOutput("vstock_list")))),
             
             tabPanel("Optimalization Method", value = "methodpanel",
                      headerPanel("Choose optimalizations and constraints"),
                      sidebarPanel(
                        selectInput("shorting", "Allow for shorting?",
                                    c("Yes", "No"), selected = "No"),
                        selectInput("method", "Optimalization method:",
                                    c("Sharpe Ratio Maximizing", 
                                      "Volatility Minimizing", 
                                      "Sortino Ratio Maximizing"),
                                    selected = "Sortino Ratio Maximizing"),
                        
                        numericInput("stockmax", "Max ratio of a stock in the portfolio:", 
                                     value = 1,
                                     min = 0,
                                     max = 1,
                                     step = 0.01
                        ),
                        
                        numericInput("stockmin", "Min ratio of a stock in the portfolio", 
                                     value = 0,
                                     min = 0,
                                     max = 1,
                                     step = 0.01),
                    
                        actionButton("update2", "Confirm")), 
                      
                      
                      
                      
                      mainPanel(
                        h3("About the methods:"),
                        
                        
                        h4("Maximizing Sharpe ratio: Maximizing the return/risk beyond the risk-free-rate"),
                        p("+ Sharpe ratio ajust the portofolio based upon the risk beyond the risk-free-rate"),
                        p("- Sharpe ratio assumes that investment returns are normal distributed, which is not always true"),
                        p("- In the sharpe ratio risk is equal to volatility"),
                        tags$a(href="https://www.investopedia.com/terms/s/sharperatio.asp", "Learn more about sharpe ratio"),
                        
                        h4("Minimizing Volatility: Minimizing the stocks swing around the mean price"),
                        p("+ Creates a portofolio which is highly predictable and stable"),
                        p("- You may miss out on higher returns due to the method not favouring sudden good or bad news"),
                        tags$a(href="https://www.investopedia.com/terms/v/volatility.asp", "Learn more about volatility"),
                        
                        h4("Sortino: Differentating hearmful volatility from the total overall volatility"),
                        p("+ Only considers the standard deviation of the downside risk, thus valuing positive volatility"),
                        p("+ You get a maximized return from the downside risk"),
                        tags$a(href="https://www.investopedia.com/terms/s/sortinoratio.asp", "Learn more about the sortino ratio")
                        
                        
                      )),
             
             
             
             tabPanel("Results", value = "resultspanel",
                      headerPanel("Your optimal portofolio"),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Portofolio stats",
                                   
                                   # Portfolio stats
                                   h3("General Stats"),
                                   
                                   shinycssloaders::withSpinner(tableOutput("stats")), # This needs to be dynamic and show the stats from the chosen method
                                   
                                   h3("Optimal Volume"),
                                   shinycssloaders::withSpinner(dataTableOutput("volstats")),
                                   
                                   # port stats
                                   # h3("Vol min stats"),
                                   # shinycssloaders::withSpinner(tableOutput("vopt_stat")),
                                   
                                   # Portfolio stats
                                   # h3("Sharpe max stats"),
                                   # shinycssloaders::withSpinner(tableOutput("vsharpe_stat")),
                                   
                                   # portfolio stats
                                   # h3("Stortino max stats"),
                                   # shinycssloaders::withSpinner(tableOutput("vsortino_stat")),
                                   
                                   #sortino
                                   # h4("Sortino ratio"), 
                                   # shinycssloaders::withSpinner(dataTableOutput("vsortino")),
                                   
                                   
                          ),
                          tabPanel("Charts",
                                   
                                   # Stock Correlation 
                                   h4("Portfolio"),
                                   shinycssloaders::withSpinner(plotOutput("vpiechart")),
                                   
                                   #portfolio returns historgram
                                   h4("Portfolio returns histogram"),
                                   shinycssloaders::withSpinner(plotOutput("vport_hist")),
                                   
                                   # Returns histogram
                                   #h4("Returns Histogram"),
                                   #shinycssloaders::withSpinner(plotOutput("vreturns_hist")),
                                   
                                   
                                   
                                   # Efficiency frontier
                                   #h4("Efficiency Frontier"),
                                   #shinycssloaders::withSpinner(plotOutput("vefficency_frontier")),
                                   
                                   # Comparison with S&P500
                                   h4("S&P500 Comparison"),
                                   shinycssloaders::withSpinner(plotOutput("vcompare_SP500"))
                          ),
                          tabPanel("Extras", 
                                   
                                   # Stock Correlation 
                                   h4("Stock Correlation"),
                                   shinycssloaders::withSpinner(plotOutput("vcorr_plot")),
                                   
                                   # Stock price history
                                   h4("Stock Price History"),
                                   shinycssloaders::withSpinner(plotOutput("vstock_price_history")), 
                                   
                                   # Returns histogram
                                   h4("Returns Histogram"),
                                   shinycssloaders::withSpinner(plotOutput("vreturns_hist")),
                                   
                                   # Efficiency frontier
                                   h4("Efficiency Frontier"),
                                   shinycssloaders::withSpinner(plotOutput("vefficency_frontier")),
                                   
                                   h4("Soon to be added")))
                        
                        
                        # portfolio stats
                        
                        
                        
                        
                        
                        
                        
                        
                        
                      ) # main panel
             ) # tab panel
             
             , position = "fixed-top"), 
  # navbarPage
  
) # UI



server <- function(input, output, session) {
  
  
  
  ############################### PROCESSING INPUTS ###############################################
  
  # Update stock_input with input from dateInput "fromdate" og "todate"
  # eventReactive means that it updates the function inputs every time user clicks the "update" button
  
  #  observe({  # Update the choice list - currently only work when the user select one industry. It does not filter out if the user select more than one industry (needs to be fixed)
  #    updateSelectInput(session, "manual", 
  #                      choices = stocks_with_industry$Symbol[stocks_with_industry$Industry != input$industry]) # The user will not be able to chose a stock within the unwanted industry
  #                      
  #  })
  
  `%notin%` <- Negate(`%in%`) # Making an opposite of %in%
  
  
  # Warning 1 - If number of unique stocks is beyond 50 a warning notification about loading time will show
  observe({
    
    if (!isTruthy(input$n_unique_stocks)){
      showNotification(id = "noinput1", "Please select a number of stocks", type = "error")
    }
    else if (input$n_unique_stocks > 50 & input$n_unique_stocks <= max_stockselection){
      showNotification(id = "above50", "You have selected more than 50 stocks, expect 1 minute of loading time", type = "warning")
    }
    else if(input$n_unique_stocks > max_stockselection){
      showNotification(id = "above100", "Please select less than 100 stocks", type = "error")
    }
  
    else{
      NULL
    }
  })
  
  observe({
    if (!isTruthy(input$rfrate)){
      showNotification(id = "noRfRate", "Please specify risk free rate", type = "error")
    }
    else{
      NULL
    }
  })
  
#  observe({
#    if (!isTruthy(input$manual)){
#      showNotification(id = "noStocks", "Please select stocks", type = "default")
#    }
#    else{
#      NULL
#    }
#  })
  
  # Warning 2 - No from or to date
  observe({
    if (!isTruthy(input$fromdate)){
      showNotification(id = "NoFromDate", "Please specify which date you want the portfolio optimization to start from", type = "error")
    }
    else{
      NULL
    }
  })
  
  observe({
    if (!isTruthy(input$todate)){
      showNotification(id = "NoToDate", "Please specify which date you want the portfolio optimization to start from", type = "error")
    }
    else{
      NULL
    }
  })
  
  
 
  
  

  # If sinstocks are unwanted -> make a subset of the dataframe without sinstocks
  sin.choice <- reactive({
    sinstocks <- c("marked as unethical ")
    if (isTRUE(input$sinstock == "Yes")){
      return(stocks_with_industry[stocks_with_industry$Ethics %notin% sinstocks,])

    }
    else{
      return(stocks_with_industry[stocks_with_industry$Industry %notin% input$industry,])
    }
  })
  
  # Incorporate the sinstock subset with the green.stock only choice 
  choice1 <-  reactive({
    if (isTRUE(input$greenonly == "Yes")){
      sin.choice()$Symbol[sin.choice()$Ethics == " Marked as green stock" & sin.choice()$Industry %notin% input$industry]
      
    }
    else{
      sin.choice()$Symbol[sin.choice()$Industry %notin% input$industry]
    }
  })
  

  
#  choice1 <-  reactive({
#    stocks_with_industry$Symbol[stocks_with_industry$Industry %notin% input$industry]
#  })
  
  
  observe({  # Update the choice list - currently only work when the user select one industry. It does not filter out if the user select more than one industry (needs to be fixed)
    updateSelectInput(session, "manual", 
                      choices = choice1()) # The user will not be able to chose a stock within the unwanted industry
    
  })
  
  observeEvent(input$random, {  # Get random portfolio which does not include chosen undesired industry - the user can select the amount of stocks in the random portfolio
    
    updateSelectInput(session, "manual",
                      selected = sample(choice1(), input$n_unique_stocks))
  })
  
  
  
  # Update min value in numeric input based on the amount of stocks you select to the portfolio
  observe({
    if (isTRUE(input$shorting == "Yes")){
      updateNumericInput(session, "stockmax", 
                         value = 1,
                         min = 1,
                         max = 1)
      
    }
    else{
      updateNumericInput(session, "stockmax", 
                         value = 1,
                         min = 1/length(input$manual),
                         max = 1)
    }
  })
  
  # Update max value in numeric input based on the amount of stocks you select to the portfolio
  observe({
    
    if (isTRUE(input$shorting == "Yes")){
      updateNumericInput(session, "stockmin",
                         value = -1,
                         max = -1,
                         min = -1,
                         )

      
    }
    else{
      updateNumericInput(session, "stockmin", 
                         value = 0,
                         max = 1/length(input$manual),
                         min = 0)
    }
    
  })
  
  # Making the risk_free_rate global
  risk_free_rate <<- eventReactive(c(input$update, input$update2), {
    risk_free_rate <- input$rfrate
  }, ignoreNULL = FALSE)
  
  
  tickers1 <- eventReactive(c(input$update, input$update2), {
    input$manual
  }, ignoreNULL = FALSE)
  
  
  # Go to next page when a confirming button is pressed
  
  observeEvent(input$update, {
    updateTabsetPanel(session, "tabset1", 
                      selected = "methodpanel")
  })
  
  observeEvent(input$update2, {
    updateTabsetPanel(session, "tabset1",
                      selected = "resultspanel")
  })
  
  
  
  # -- Making a general function for user inputs to be applied to stock_input
  
  dataInput <- eventReactive(c(input$update, input$update2), {
    input_function(tickers1(), input$fromdate, input$todate)
  }, ignoreNULL = FALSE)
  
  
  
  # -- Making another input function to be able to use subsets from the stock_opt_vol and stock_opt_sharpe functions.
  
  vol_output <- eventReactive(c(input$update, input$update2), {
    stock_opt_vol(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]], as.numeric(input$stockmax), as.numeric(input$stockmin))
  }, ignoreNULL = FALSE)
  
  sharpe_output <- eventReactive(c(input$update, input$update2), {
    stock_opt_sharpe(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]], as.numeric(input$stockmax), as.numeric(input$stockmin))
  }, ignoreNULL = FALSE)
  
  sortino_output <- eventReactive(c(input$update, input$update2), { 
    stock_opt_sortino(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]], as.numeric(input$stockmax), as.numeric(input$stockmin))
  }, ignoreNULL = FALSE)
  
  ############ END OF INPUT PROCESSING ##########
  
  
  
  
  ########### GENERATING OUTPUTS #############
  # Generate output for available stocks
  output$vstock_list <-renderDataTable({
    stocks_with_industry # Here we output a subset of vol_input
  })
  
  
  
  
  # Generate output for stats based upon chosen method
  
  output$stats <- renderTable({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    x <- input$method
    if (x == "Sharpe Ratio Maximizing"){
      sharpe_output()[[4]]
    }
    else if (x == "Sortino Ratio Maximizing"){
      sortino_output()[[4]]
    }
    else{
      vol_output()[[4]]
    }
  })
  
  # Generate output for optimal volume based upon chosen method
  
  output$volstats <- renderDataTable({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    x <- input$method
    if (x == "Sharpe Ratio Maximizing"){
      sharpe_output()[[3]]
    }
    else if (x == "Sortino Ratio Maximizing"){
      sortino_output()[[3]]
    }
    else{
      vol_output()[[3]]
    }
  })
  
  
  # Generate output for portfolio industry percentages
  output$vpiechart <- renderPlot({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    x <- input$method
    if (x == "Sharpe Ratio Maximizing"){
      portfolio_industry(dataInput()[[1]], (sharpe_output()[[2]]))
    }
    else if (x == "Sortino Ratio Maximizing"){
      portfolio_industry(dataInput()[[1]], (sortino_output()[[2]]))
    }
    else{
      portfolio_industry(dataInput()[[1]], (vol_output()[[2]]))
    }
    
  })
  
  
  # Generate output for the correlation plot
  output$vcorr_plot <- renderPlot({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    
  x <- input$method
  if (x == "Sharpe Ratio Maximizing"){
    correlation_plot(sharpe_output()[[3]], input$fromdate, input$todate)
  }
  else if (x == "Sortino Ratio Maximizing"){
    correlation_plot(sortino_output()[[3]], input$fromdate, input$todate)
  }
  else{
    correlation_plot(vol_output()[[3]], input$fromdate, input$todate)
  }
  
  })
    
    

  
  # Generate output for the returns histogram
  output$vreturns_hist <- renderPlot({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
      x <- input$method
      if (x == "Sharpe Ratio Maximizing"){
        returns_hist(dataInput()[[5]], sharpe_output()[[3]])
      }
      else if (x == "Sortino Ratio Maximizing"){
        returns_hist(dataInput()[[5]],sortino_output()[[3]])
      }
      else{
        returns_hist(dataInput()[[5]],vol_output()[[3]])
      }
      
  })
    
  
  # Genereate output for the stock price history
  output$vstock_price_history <- renderPlot({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    x <- input$method
    if (x == "Sharpe Ratio Maximizing"){
      stock_price_history(dataInput()[[2]],sharpe_output()[[3]],input$fromdate, input$todate)
    }
    else if (x == "Sortino Ratio Maximizing"){
      stock_price_history(dataInput()[[2]],sortino_output()[[3]],input$fromdate, input$todate)
    }
    else{
      stock_price_history(dataInput()[[2]],vol_output()[[3]],input$fromdate, input$todate)
    }
    
  })
  
  # Generate output for the efficiency frontier
  output$vefficency_frontier <- renderPlot({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    efficency_frontier(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]], n = 5000)
  })
  
  # Generate output for the S&P500 comparison
  output$vcompare_SP500 <- renderPlot({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    x <- input$method
    if (x == "Sharpe Ratio Maximizing"){
      compare_SP500(as.matrix(sharpe_output()[[2]]), dataInput()[[1]], input$fromdate, input$todate)
    }
    else if (x == "Sortino Ratio Maximizing"){
      compare_SP500(as.matrix(sortino_output()[[2]]), dataInput()[[1]], input$fromdate, input$todate)
    }
    else{
      compare_SP500(as.matrix(vol_output()[[2]]), dataInput()[[1]], input$fromdate, input$todate)
    }
    
  })
  
  # Generate output for the portfolio returns histogram
  output$vport_hist <- renderPlot({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    returns_final_hist(dataInput()[[3]],as.matrix(sharpe_output()[[2]]))
  })
  
  
}

shinyApp(ui = ui, server = server) # Combine it into the app

# Depending on the amount of stocks there can be some loading time (from 20 sec to 3 minutes)

