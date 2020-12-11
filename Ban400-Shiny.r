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


# Setting a default date
from_date <- "2017-05-01"
to_date <- "2020-08-31"

# Setting a dynamic default max date
max_to_date <- Sys.Date()
max_from_date <- as.Date(max_to_date) - 7 # Ensuring that max from date never is less than 7 days before the to date


# Setting a default max value for stock selection
max_stockselection <- 1000


ui <- fluidPage(
  tags$main(tags$style(css)),
  tags$header(tags$style(css2)),
  theme = shinytheme("cosmo"),
  
  
  navbarPage(title="Portfolio Optimizer", id = "tabset1",
             
             tabPanel("Stock Selection",
                      
                      headerPanel('Find your optimal portofolio'),
                      sidebarPanel(
                        
                        numericInput("n_unique_stocks", "Amount of stocks to select a portfolio from: ", 10,
                                     min = 2, 
                                     max = max_stockselection,
                                     step = 1),
                        
                        selectInput("sinstock", "Avoid sinstocks?", c("Yes", "No"), "Yes"),
                        
                        
                        selectInput("greenonly", "Green Stocks Only?", c("Yes", "No"), "No"),
                        
                        selectInput("selectionType", "Select unfit industries or select industries?", 
                                    c("Select industries", "Select unfit industries"), 
                                    selected = "Select unfit industries"),
                        
                        # These panels will only show based upon selection of "selectionType"
                        conditionalPanel(
                          condition = "input.selectionType == 'Select unfit industries'",
                          selectInput("industry", "Select unfit industries:", stocks_with_industry$Industry, multiple = TRUE, selectize = TRUE
                          )),
                        conditionalPanel(
                          condition = "input.selectionType == 'Select industries'",
                          selectInput("industrySelect", "Select industries to invest in: ", stocks_with_industry$Industry, multiple = TRUE, selectize = TRUE)
                          
                        ),
                        
                        actionButton("random", "Get portfolio"), # Goal: Refresh a random selection of stocks within the selected categories
                        selectInput("manual", "Select stocks", stocks_with_industry$Symbol, multiple = TRUE, selectize = TRUE),
                        
                        numericInput("rfrate", "Risk free rate: ", 0.02,
                                     min = 0, 
                                     max = 1,
                                     step = 0.001),
                        
                        
                        dateInput("fromdate", "Test-data from: ", 
                                  from_date,
                                  min = "2000-01-01",
                                  max = max_from_date, # We need minimum 1 week of data to give sensible answers
                                  format = "yyyy/mm/dd"
                        ),
                        dateInput("todate", "Test-data to: ",
                                  to_date,
                                  min = "2000-01-02",
                                  max = max_to_date,
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
                                    selected = "Sharpe Ratio Maximizing"),
                        
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
             
             # Shinycssloaders is the loading animation
             
             tabPanel("Results", value = "resultspanel",
                      headerPanel("Your optimal portofolio"),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Portofolio stats",
                                   
                                   # Portfolio stats
                                   h3("General Stats"),
                                   
                                   shinycssloaders::withSpinner(tableOutput("stats")), 
                                   
                                   h3("Optimal Volume"),
                                   shinycssloaders::withSpinner(dataTableOutput("volstats")),
                                   
                                   
                                   
                          ),
                          tabPanel("Charts",
                                   
                                   # Stock Correlation 
                                   h4("Portfolio"),
                                   shinycssloaders::withSpinner(plotOutput("vpiechart")),
                                   
                                   #portfolio returns historgram
                                   h4("Portfolio returns histogram"),
                                   shinycssloaders::withSpinner(plotOutput("vport_hist")),
                                   
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
                                   
                                   ) # Tabpanel
                          ) # Tabset panel
                      ) # main panel
             ) # tab panel
             , position = "fixed-top"), # navbarPage
  
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
  
  # Error 1 - Check if risk free rate is specified
  observe({
    if (!isTruthy(input$rfrate)){
      showNotification(id = "noRfRate", "Please specify risk free rate", type = "error")
    }
    else{
      NULL
    }
  })
  
  
  # Error 2 - No from date
  observe({
    if (!isTruthy(input$fromdate)){
      showNotification(id = "NoFromDate", "Please specify which date you want the portfolio optimization to start from", type = "error")
    }
    else{
      NULL
    }
  })
  
  # Error 3 - No to date
  observe({
    if (!isTruthy(input$todate)){
      showNotification(id = "NoToDate", "Please specify which date you want the portfolio optimization to start from", type = "error")
    }
    else{
      NULL
    }
  })
  
  observe({
    if (length(input$manual) > 50){
      showNotification("Loading may now take up to 1 minute", type = "warning")
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
        return(stocks_with_industry)
      }
  })
  
  # Incorporate the sinstock subset with the green.stock only choice 
  choice1 <-  reactive({
    if (isTRUE(input$greenonly == "Yes")){
      if(isTRUE(input$selectionType == "Select unfit industries")){
        
        sin.choice()$Symbol[sin.choice()$Ethics == " Marked as green stock" & sin.choice()$Industry %notin% input$industry]
      }
      else{
        sin.choice()$Symbol[sin.choice()$Ethics == " Marked as green stock" & sin.choice()$Industry %in% input$industrySelect]
      }
      
    }
    else{
      if(isTRUE(input$selectionType == "Select unfit industries")){
        sin.choice()$Symbol[sin.choice()$Industry %notin% input$industry]
      }
      else{
        sin.choice()$Symbol[sin.choice()$Industry %in% input$industrySelect]
      }
    }
  })
  
  # Make a subset if sinstocks is unwanted
  sin.choice.industry <- reactive({         
    sinstocks <- c("marked as unethical ")
    
    if (isTRUE(input$sinstock == "Yes")){
      return(stocks_with_industry[stocks_with_industry$Ethics %notin% sinstocks,])
    }
    else{
        return(stocks_with_industry)
      }
    
  })
  
  # Use the subset from previous selection to further subset the possible choices if "green only" is selected
  industryChoice <- reactive({
    if (isTRUE(input$greenonly == "Yes")){
        sin.choice.industry()$Industry[sin.choice.industry()$Ethics == " Marked as green stock"]
      }
    else{
        sin.choice.industry()$Industry
      }
    
  })
  
  
  # Update industry choice list for both selections
  observe({
    updateSelectInput(session, "industry",
                      choices = industryChoice())
  })
  
  observe({
    updateSelectInput(session, "industrySelect",
                      choices = industryChoice())
  })
  
  
  observe({  # Update the choice list - currently only work when the user select one industry. It does not filter out if the user select more than one industry (needs to be fixed)
    updateSelectInput(session, "manual", 
                      choices = choice1()) # The user will not be able to chose a stock within the unwanted industry
    
  })
  
  
  observeEvent(input$random, {  # Get random portfolio which does not include chosen undesired industry - the user can select the amount of stocks in the random portfolio
    if (isTRUE(input$n_unique_stocks <= length(choice1()))){ # Check if amount of random stocks to draw is less or equal than the available stocks
      updateSelectInput(session, "manual",
                        selected = sample(choice1(), input$n_unique_stocks))
    }
    else{
      showNotification(id = "randomWarning", "Not enough stocks aviable, select less stocks or select more industries", type = "error")
    }
    
  })
  
  # -- Add warning when stock ratio input value is illigal
  observe({
    if (!isTruthy(input$stockmax)){
      showNotification("Please insert a number in max stock ratio", type = "error")
    }
    else if (isTRUE(input$stockmax > 1)){
    showNotification("Please insert a max stock ratio equal or lower than 1", type = "error")
  
    }
    else{
      NULL
    }
  })
  
  observe({
    if (!isTruthy(input$stockmin)){
      showNotification("Please insert a number in stock min ratio", type = "error")
    }
    
    else if (isTRUE(input$stockmin < -1)){
      showNotification("Please insert a stock min ratio equal or higher than -1", type = "error")
    }
    else{
      NULL
    }
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
  
  # -- Making the risk_free_rate global
  risk_free_rate <<- eventReactive(c(input$update, input$update2), {
    risk_free_rate <- input$rfrate
  }, ignoreNULL = FALSE)
  
  # -- When update or confirm is clicked -> update the selected tickers list
  tickers1 <- eventReactive(c(input$update, input$update2), {
    input$manual
  }, ignoreNULL = FALSE)
  
  
  
  
  # Give user a message if there has been selected few stocks
  observeEvent(input$update, {
    if(length(input$manual) < 5){
      showNotification("You may get an error, if so then select more stocks or other stocks", type = "message")
    }
    else{
      NULL
    }
  })
  
  
  # -- If input is correct -> Go to next page when a confirming button is pressed
  
  # -- If input is not valid -> Make the user correct input values before sending the user to the next page
  observeEvent(input$update, {
    
    if (!isTruthy(input$rfrate)){
      showNotification("Please write a valid risk free rate", type = "error")
    }
    else if (!isTruthy(input$fromdate)){
      showNotification("Please write a valid date", type = "error")
    }
    else if(!isTruthy(input$todate)){
      showNotification("Please write a valid date", type = "error")
    }
    else if(!isTruthy(input$manual)){
      showNotification("Please choose at least 2 stocks", type = "error")
    }
    else if(length(input$manual) < 2){
      showNotification("Please select at least 2 but prefferably 5 or more stocks", type = "error")
    }
    else{
      updateTabsetPanel(session, "tabset1", 
                        selected = "methodpanel")
    }
  
  })
  
  # -- If input is correct -> Go to next page when a confirming button is pressed
  
  # -- If input is not valid -> Make the user correct input values before sending the user to the next page
  observeEvent(input$update2, {
    if (!isTruthy(input$stockmax)){
      showNotification("Please insert a number in max stock ratio", type = "error")
    }
    else if (isTRUE(input$stockmax > 1)){
      showNotification("Please insert a max stock ratio equal or lower than 1", type = "error")
    }
    else if (isTRUE(input$stockmax < 1/length(input$manual))){
      showNotification("Please select a higher stock max value", type = "error")
    }
    else if (!isTruthy(input$stockmin)){
      showNotification("Please insert a number in stock min ratio", type = "error")
    }
    
    else if (isTRUE(input$stockmin < -1)){
      showNotification("Please insert a stock min ratio equal or higher than -1", type = "error")
    }
    else{
      updateTabsetPanel(session, "tabset1",
                        selected = "resultspanel")
    }
    
   
  })
  
  
  
  # -- Making a general function for user inputs to be applied to stock_input in our imported Ban400-Functions script
  
  dataInput <- eventReactive(c(input$update, input$update2), {
    input_function(tickers1(), input$fromdate, input$todate)
  }, ignoreNULL = FALSE)
  
  
  # dataInput 1 = tickers
  # dataInput 2 = stock_prices
  # dataInput 3 = returns_matrix
  # dataInput 4 = stock_correlation
  # dataInput 5 = stock_return with date
  # dataInput 6 = stock_covariance
  # dataInput 7 = portfolio weigths
  # dataInput 8 = errors
  # dataInput 9 = stock info
  
  
  # -- Making input functions to be able to use subsets from the stock_opt_vol, stock_opt sortino and stock_opt_sharpe functions.
  
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
  
  
  
  ############################################
  ########### GENERATING OUTPUTS #############
  ############################################
  
  # Generate output for available stocks
  output$vstock_list <-renderDataTable({
    stocks_with_industry # Here we output a subset of vol_input
  })
  
  # Before generating output for stats based upon chosen method - check that there is input
  # If there is no input -> display a error message where the user is informed about what the missing input is
  # The validation is done for all the outputs to present a coherent message when something is going wrong
  
  output$stats <- renderTable({
    nstocks <- length(input$manual)
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
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
    nstocks <- length(input$manual)
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
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
    nstocks <- length(input$manual)
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
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
    nstocks <- length(input$manual)
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
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
    nstocks <- length(input$manual)
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
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
    nstocks <- length(input$manual)
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
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
    nstocks <- length(input$manual)
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    efficency_frontier(dataInput()[[1]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]], n = 5000)
  })
  
  # Generate output for the S&P500 comparison
  output$vcompare_SP500 <- renderPlot({
    nstocks <- length(input$manual)
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
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
    nstocks <- length(input$manual)
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    returns_final_hist(dataInput()[[3]],as.matrix(sharpe_output()[[2]]))
  })
  
  
}

shinyApp(ui = ui, server = server) # Combine it into the app

# Depending on the amount of stocks there can be some loading time (from 10 sec to 3 minutes)

