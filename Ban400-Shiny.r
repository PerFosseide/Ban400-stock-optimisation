############# BAN400 #############
####### STOCK OPTIMIZER ##########
############ ~ V1.0 ~ ############


# -- Load libraries to use -- # 

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

# -- Load stock optimizing functions

source("Ban400-Shiny-Functions.R")

# -- End of loading -- #


# -- CSS and styling add-ons #

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

# -- End of CSS add-ons #



# -- Default values -- #

# Setting a default date
from_date <- "2017-05-01"
to_date <- "2020-07-31"

# Setting dynamic default max dates
max_to_date <- Sys.Date()
max_from_date <- as.Date(max_to_date) - 7 # Ensuring that max from date is not closer than 7 days

# Setting a default max value for stock selection
max_stockselection <- 1000

# -- End of default values -- #

# -- start of the user interface -- #



# Shinycssloaders is the loading animation

ui <- fluidPage(
  tags$main(tags$style(css)), # Adding the CSS add-ons
  tags$header(tags$style(css2)), 
  theme = shinytheme("cosmo"), # Setting a shiny-theme (which is based on CSS)
  
  
  navbarPage(title="Portfolio Optimizer", id = "tabset1", 
             
             tabPanel("Stock Selection", # Dividing the UI into tabpanels to simplify the user experience
                      
                      headerPanel('Find your optimal portofolio'), # Header on the page
                      sidebarPanel( # Dynamic content wrapper
                        
                        numericInput("n_unique_stocks", "Amount of stocks to select a portfolio from: ", 10, # Default value is 10
                                     max = max_stockselection, # Dynamic max value (see dynamic values in the beginning)
                                     step = 1), # Add or subtract one when user changes input with the arrows
                        
                        # An option input where user can choose to keep or avoid sin-stocks in the portfolio
                        selectInput("sinstock", "Avoid sinstocks?", c("Yes", "No"), "Yes"), 
                        
                        # An option input where user can choose to only get green stocks or not
                        selectInput("greenonly", "Green Stocks Only?", c("Yes", "No"), "No"),
                        
                        # An option input where user can choose to select industries or select unwanted (unfit for investment) industries
                        selectInput("selectionType", "Select unfit industries or select industries?", 
                                    c("Select industries", "Select unfit industries"), # These are the option presented to the user
                                    selected = "Select unfit industries"), # This is the default choice
                        
                        # The conditional panels will only show based upon selection of "selectionType"
                        conditionalPanel(
                          condition = "input.selectionType == 'Select unfit industries'", # If user select the possibility to choose unfit industries:
                          selectInput("industry", "Select unfit industries:", # Show the option to select unfit industries
                                      stocks_with_industry$Industry, # Default available options
                                      multiple = TRUE,  # Allow multiple industries to be selected
                                      selectize = TRUE # Adding javascript functionality -> makes the input box easy to use
                          )),
                        conditionalPanel(
                          condition = "input.selectionType == 'Select industries'", # If user select the possibility to choose industries:
                          selectInput("industrySelect", "Select industries to invest in: ", # Show the option to select industries
                                      stocks_with_industry$Industry, # Default available options
                                      multiple = TRUE,  # Allow multiple industries to be selected
                                      selectize = TRUE) # Adding javascript functionality -> makes the input box easy to use
                          
                        ),
                        
                        actionButton("random", "Get portfolio"), # Get a portfolio aligned with your previous choices
                        selectInput("manual", "Select stocks", 
                                    stocks_with_industry$Symbol, # Default available options
                                    multiple = TRUE, # Allow multiple industries to be selected
                                    selectize = TRUE), # Adding javascript functionality -> makes the input box easy to use
                        
                        numericInput("rfrate", "Risk free rate: ", 0.02, # Default risk free rate = 0.02
                                     min = 0, # min allowed value if you use the arrows on the input box
                                     max = 1, # max allowed value if you use the arrows on the input box
                                     step = 0.001), # How large increments or decrements when clicking the arrows in the input box
                        
                        
                        dateInput("fromdate", "Test-data from: ", 
                                  from_date, # Default choice - see default values in the beginning of this script for more info
                                  min = "2000-01-01", # Min allowed value when using the arrows on the input box
                                  max = max_from_date, # Dynamic max - see default values in the beginning of the script for more info
                                  format = "yyyy/mm/dd" # The date-format that is valid
                        ),
                        dateInput("todate", "Test-data to: ", 
                                  to_date, # Default choice - see default values in the beginning of this script for more info
                                  min = "2000-01-02", # Min allowed value when using the arrows on the input box
                                  max = max_to_date, # Dynamic max - see default values in the beginning of the script for more info
                                  format = "yyyy/mm/dd" # The date-format that is valid
                        ),
                        
                        # Action button to confirm selection and avoid constant processing when values are changed in the sidebarPanel
                        actionButton("update", "Confirm Selection")), 
                      
                      # A dynamic content box
                      mainPanel( 
                        # All available stocks
                        h3("Available Stocks"), # Just a header in h3 size
                        
                        # Show the list over available stocks - show loading animation when loading
                        shinycssloaders::withSpinner(dataTableOutput("vstock_list")))), 
                    
             tabPanel("Optimalization Method", value = "methodpanel", # New tab panel - value is used to have an ID to refer to the tab
                      headerPanel("Choose optimalizations and constraints"), # Header
                      sidebarPanel( # New dynamic sidebar content wrapper
                        
                        selectInput("shorting", "Allow for shorting?", # Option where user is asked if shorting is desired or not
                                    c("Yes", "No"), # The available options
                                    selected = "No"), # The default choice
  
                        selectInput("method", "Optimalization method:", # Input for which optimalization method to use
                                    c("Sharpe Ratio Maximizing", # The available options
                                      "Volatility Minimizing", 
                                      "Sortino Ratio Maximizing"),
                                    selected = "Sharpe Ratio Maximizing"), # The default choice
                        
                        numericInput("stockmax", "Max ratio of a stock in the portfolio:", # Input where user select max ratio of a stock in portfolio
                                     value = 1, # Default value
                                     min = 0,
                                     max = 1,
                                     step = 0.01 
                        ),
                        
                        numericInput("stockmin", "Min ratio of a stock in the portfolio", # Input where user select min ratio of a stock in portfolio
                                     value = 0, # Default value
                                     min = 0,
                                     max = 1,
                                     step = 0.01),
                        
                        actionButton("update2", "Confirm")), # Update button to confirm choices and update the functions with the content
                      
                      
                      
                      # A dynamic content wrapper where the text is placed
                      # H3, H4, p etc. the size and type of the text where p is regular text formatting and Hx header size
                      mainPanel(
                        h3("About the methods:"),
                        
                        
                        h4("Maximizing Sharpe ratio: Maximizing the return/risk beyond the risk-free-rate"),
                        p("+ Sharpe ratio ajust the portofolio based upon the risk beyond the risk-free-rate"),
                        p("- Sharpe ratio assumes that investment returns are normal distributed, which is not always true"),
                        p("- In the sharpe ratio risk is equal to volatility"),
                        tags$a(href="https://www.investopedia.com/terms/s/sharperatio.asp", "Learn more about sharpe ratio"), # link to webpage
                        
                        h4("Minimizing Volatility: Minimizing the stocks swing around the mean price"),
                        p("+ Creates a portofolio which is highly predictable and stable"),
                        p("- You may miss out on higher returns due to the method not favouring sudden good or bad news"),
                        tags$a(href="https://www.investopedia.com/terms/v/volatility.asp", "Learn more about volatility"), # link to webpage
                        
                        h4("Sortino: Differentating hearmful volatility from the total overall volatility"),
                        p("+ Only considers the standard deviation of the downside risk, thus valuing positive volatility"),
                        p("+ You get a maximized return from the downside risk"),
                        tags$a(href="https://www.investopedia.com/terms/s/sortinoratio.asp", "Learn more about the sortino ratio") # link to webpage
                        
                        
                      )),
            
             
             tabPanel("Results", value = "resultspanel", # The third and final tab where the results are displayed
                      headerPanel("Your optimal portofolio"), # Header text
                      mainPanel( # Dynamic content wrapper
                        tabsetPanel( # The page is divided into 3 tabsets - 1) Portfolio stats, 2) Charts, and 3) Extras
                          tabPanel("Portofolio stats", # First tabset page
                                   
                                   # Portfolio stats
                                   h3("General Stats"),
                                   
                                   shinycssloaders::withSpinner(tableOutput("stats")), # Show statistics about the optimal portfolio
                                   
                                   # Optimal portfolio stock volumes
                                   h3("Optimal Volume, only shows positions that are larger than 0.1 %"),
                                   shinycssloaders::withSpinner(dataTableOutput("volstats")), # Show optimal volume output
                                   
                                   
                          ),
                          tabPanel("Charts",
                                   
                                   # Stock Correlation 
                                   h4("Portfolio"),
                                   shinycssloaders::withSpinner(plotOutput("vpiechart")), # Show piechart of the optimal portfolio
                                   
                                   # Portfolio returns histogram
                                   h4("Portfolio returns histogram"),
                                   shinycssloaders::withSpinner(plotOutput("vport_hist")), # Show histogram of the optimal portfolio
                                   
                                   # Comparison with S&P500
                                   h4("S&P500 Comparison"),
                                   shinycssloaders::withSpinner(plotOutput("vcompare_SP500")) # Show comparison with S&P500
                          ),
                          tabPanel("Extras", 
                                   
                                   # Stock Correlation 
                                   h4("Stock Correlation"),
                                   shinycssloaders::withSpinner(plotOutput("vcorr_plot")), # Show stock correlation of stocks within the portfolio
                                   
                                   # Stock price history
                                   h4("Stock Price History"),
                                   shinycssloaders::withSpinner(plotOutput("vstock_price_history")),  # Show stock price history of the portfolio stocks
                                   
                                   # Returns histogram
                                   h4("Returns Histogram"),
                                   shinycssloaders::withSpinner(plotOutput("vreturns_hist")), # Show returns histogram
                                   
                                   # Efficiency frontier
                                   h4("Efficiency Frontier"),
                                   shinycssloaders::withSpinner(plotOutput("vefficency_frontier")), # Show efficiency frontier of the possible combinations
                                   
                                   ) # Tabpanel
                          ) # Tabset panel
                      ) # main panel
             ) # tab panel
             , position = "fixed-top"), # navbarPage
  
) # UI

#### END OF UI #####



####### START OF SERVER (The backend of the shiny app) ######


server <- function(input, output, session) { 
  
  # Input = input from UI
  # Output = Output from the server function
  # Session = To keep track of the session
  
  
  ############################### INPUT PROCESSING ###############################################
  
  
  # Here we process inputs from UI to update the general input function in 
  # "Ban400-Functions", stock_input, with input from dateInput "fromdate" og "todate".
  
  # Reactive means that the function responds to user changes in UI
  # Observe means that the function is looking for changes in the UI
  
  # eventReactive means that it updates the function inputs every time user clicks the "update" button
  # eventObserve means that it observe changes whenever a mentioned button is clicked
  

  
  `%notin%` <- Negate(`%in%`) # Making an opposite operator of %in%
  
  
  
  # ------- First some warnings -------- #
  
  # Warning 1 - If number of unique stocks is beyond 50 a warning notification about loading time will show
  observe({
    
    if (!isTruthy(input$n_unique_stocks)){ # If input$n_unique_stocks has no input (""), NA, etc.
      showNotification(id = "noinput1", "Please select a number of stocks", type = "error") # Show a notification with theme "error"
    }
    
    else if (input$n_unique_stocks > 500 & input$n_unique_stocks <= max_stockselection){ # Else if n_unique_stocks is above 50 and under max allowed number:
      showNotification(id = "above50", "You have selected a lot of stocks, expect longer loading time", type = "warning") # Show notification
    }
    else if(input$n_unique_stocks > max_stockselection){ # If number of number of stocks selected is above allowed number of stocks
      showNotification(id = "above100", "Please select less than 1000 stocks", type = "error") # Show notification
    }
    
    else{
      NULL # Do nothing
    }
  })
  
  # Error 1 - Check if risk free rate is specified
  observe({
    if (!isTruthy(input$rfrate)){ # If input$n_unique_stocks has no input (""), NA, etc.
      showNotification(id = "noRfRate", "Please specify risk free rate", type = "error")
    }
    else{
      NULL
    }
  })
  
  
  # Error 2 - No from date
  observe({
    if (!isTruthy(input$fromdate)){ # If input$n_unique_stocks has no input (""), NA, etc.
      showNotification(id = "NoFromDate", "Please specify which date you want the portfolio optimization to start from", type = "error")
    }
    else{
      NULL
    }
  })
  
  # Error 3 - No to date
  observe({
    if (!isTruthy(input$todate)){ # If input$n_unique_stocks has no input (""), NA, etc.
      showNotification(id = "NoToDate", "Please specify which date you want the portfolio optimization to start from", type = "error")
    }
    else{
      NULL
    }
  })
  
  observe({
    if (length(input$manual) > 100){ # If amount of input stocks is above 100 
      showNotification("Loading may now take up to 1 minute", type = "warning")
    }
    else{
      NULL
    }
  })
  
  
  # If sinstocks are unwanted -> make a subset of the dataframe without sinstocks
  sin.choice <- reactive({
    sinstocks <- c("marked as unethical ")
    
    if (isTRUE(input$sinstock == "Yes")){ # If input is equal to Yes 
      return(stocks_with_industry[stocks_with_industry$Ethics %notin% sinstocks,]) # Return a new subset which does not include sinstocks
    }
    else{
        return(stocks_with_industry) # Return the full set with stocks
      }
  })
  
  # Incorporate the sinstock subset with the green.stock only choice 
  choice1 <-  reactive({
    if (isTRUE(input$greenonly == "Yes")){ # If green stocks are wanted
      if(isTRUE(input$selectionType == "Select unfit industries")){ # Check if user wants to select unwanted stocks
        # Take the subset from the sin.choice and further subset it to not include green stocks or stocks from unwanted industries
        sin.choice()$Symbol[sin.choice()$Ethics == " Marked as green stock" & sin.choice()$Industry %notin% input$industry]
      }
      else{ # If user wants to select stocks
        # Take the subset from the sin.choice and further subset it to not include green stocks and only stocks from industries selected
        sin.choice()$Symbol[sin.choice()$Ethics == " Marked as green stock" & sin.choice()$Industry %in% input$industrySelect]
      }
      
    }
    else{ # If user does not only want green stocks
      if(isTRUE(input$selectionType == "Select unfit industries")){ # If user wants to select industries NOT to invest in
        # Take the subset from the sin.choice and further subset without stocks from unwanted industries
         sin.choice()$Symbol[sin.choice()$Industry %notin% input$industry] 
      }
      else{ # User want to select industries to invest in
        # Further subset the sin.choice subset to only include stocks from wanted industries
        sin.choice()$Symbol[sin.choice()$Industry %in% input$industrySelect] 
      }
    }
  })
  
  
  # Now we will do the same thing for the available options to select industries
  
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
  
  # Use the subset from previous selection to further subset the possible industry choices if "green only" is selected
  industryChoice <- reactive({
    if (isTRUE(input$greenonly == "Yes")){
        sin.choice.industry()$Industry[sin.choice.industry()$Ethics == " Marked as green stock"]
      }
    else{
        sin.choice.industry()$Industry
      }
    
  })
  
  
  # Update industry choice list for both selections based upon sin.stock selection and green stock selection
  observe({
    updateSelectInput(session, "industry",
                      choices = industryChoice())
  })
  
  observe({
    updateSelectInput(session, "industrySelect",
                      choices = industryChoice())
  })
  
  
  observe({  # Update the stock choice list
    updateSelectInput(session, "manual", 
                      choices = choice1()) # The user will not be able to chose a stock within the unwanted (or selected if that option is selected) industries
    
  })
  
  
  observeEvent(input$random, {  # Get random portfolio which does not include chosen undesired industry - the user can select the amount of stocks in the random portfolio
    if (isTRUE(input$n_unique_stocks <= length(choice1()))){ # Check if amount of random stocks to draw is less or equal than the available stocks
      updateSelectInput(session, "manual",
                        selected = sample(choice1(), input$n_unique_stocks)) # Update selected stocks with a random sample from the available stocks with n_unique_stocks new stocks.
    }
    else{ # If the number of stocks to draw are more than available stocks: show a notification warning
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
  
  # -- Add error message if minimum ratio is illigal
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
  
  
  
  observe({
    if (isTRUE(input$shorting == "Yes")){ # If shorting is wanted
      updateNumericInput(session, "stockmax", 
                         value = 1,
                         min = 1, # Update upper bounds
                         max = 1)
      
    }
    else{
      updateNumericInput(session, "stockmax", 
                         value = 1,
                         min = 1/length(input$manual), # Update min value in numeric input based on the amount of stocks you select to the portfolio
                         max = 1)
    }
  })
  
 
  observe({
    
    if (isTRUE(input$shorting == "Yes")){ # If shorting is allowed
      updateNumericInput(session, "stockmin",
                         value = -1, # Update default value
                         max = -1, # Update max value
                         min = -1, # Update min value
      )
      
      
    }
    else{ # If shorting is not allowed
      updateNumericInput(session, "stockmin", 
                         value = 0,
                         max = 1/length(input$manual),  # Update max value in numeric input based on the amount of stocks you select to the portfolio
                         min = 0)
    }
    
  })
  
  # Making the risk_free_rate global with <<-
  risk_free_rate <<- eventReactive(c(input$update, input$update2), { # Update risk free rate when either update is clicked on stock_selection page or confirm is clicked on page 2
    risk_free_rate <- input$rfrate # Risk free rate is equal to the user input
  }, ignoreNULL = FALSE) # Don't ignore no input
  
  
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
  
  
  # -- Making functions to be able to use subsets from the stock_opt_vol, stock_opt sortino and stock_opt_sharpe functions.
  
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
  
  # Before generating output for stats based upon chosen method - check that there is input
  # If there is no input -> display a error message where the user is informed about what the missing input is
  # The validation is done for all the outputs to present a coherent message when something is going wrong
  
  output$stats <- renderTable({ # A dynamic table output which generates output based upon the users method selection
    nstocks <- length(input$manual) # Number of stocks is equal to the amount of vectors (stocks) in the stock input selection
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    x <- input$method # User input from the method selection
    if (x == "Sharpe Ratio Maximizing"){ # If user has selected to use the method "Sharpe Ratio Maximizing"
      sharpe_output()[[4]] # Show sharpe output (which is retrieved from sharpe_output() subset 4).
    }
    else if (x == "Sortino Ratio Maximizing"){ 
      sortino_output()[[4]]
    }
    else{ # The user has selected "Volatility minimizing"
      vol_output()[[4]] # Show volatility minimizing
    }
  })
  
  # Generate output for optimal volume based upon chosen method
  output$volstats <- renderDataTable({
    nstocks <- length(input$manual)
    validate( # First validate if there is input 
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(nstocks >= 2, "Please select more stocks"), # Avoid the user to select too few stocks
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    x <- input$method
    if (x == "Sharpe Ratio Maximizing"){ # if user has selected sharpe ratio maximizing as method...
      sharpe_output()[[3]] # Generate sharpe ratio maximizing output
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
  output$vefficency_frontier <- renderPlot ({
    validate(
      need(isTruthy(input$rfrate), "Please input risk free rate at the stock selection page"),
      need(isTruthy(input$manual), "Please input stocks at the stock selection page"),
      need(isTruthy(input$fromdate), "Please input a valid date at the stock selection page"),  
      need(isTruthy(input$todate), "Please input a valid date at the stock selection page")
    )
    
    x <- input$method
    if (x == "Sharpe Ratio Maximizing"){
      efficency_frontier(sharpe_output()[[3]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]], n = 5000)  
    }
    else if (x == "Sortino Ratio Maximizing"){
      efficency_frontier(sortino_output()[[3]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]], n = 5000)  
    }
    else{
      efficency_frontier(vol_output()[[3]], dataInput()[[7]], dataInput()[[3]], dataInput()[[6]], n = 5000)  
    }
    
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

