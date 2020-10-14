### This will only run once ###

library(shiny)
source("Ban400-Functions.R")

##############################

ui <- fluidPage(
  headerPanel('Stockify - a stock optimizing app'),
  
  sidebarPanel(
    numericInput("rfrate", "Risk free rate: ", 0.03,
                 min = 0, 
                 max = 1,
                 step = 0.001),
    textInput("tickers", "Ticker Name: ",
              value = "AAPL",
              placeholder = "AAPL, XOM...."),
    dateInput("fromdate", "Date From: ", 
              "2005-08-01",
              min = "2005-08-01",
              max = "2020-10-01",
              format = "yyyy/mm/dd"
              ),
    dateInput("todate", "Date To: ",
              "2010-08-01",
              min = "2005-08-01",
              max = "2020-10-01",
              format = "yyyy/mm/dd"
    ),
    actionButton("refresh", "Refresh page")
#  dateRangeInput("tickerrange", "Ticker Range: ", # Possible alternative to "date-to" and "date-from"
#                 start = "2005-08-01",
#                 end = "2010-08-01",
#                 min = "2005-08-01",
#                 max = "2020-10-01",
#                 format = "yyyy/mm/dd",
#                 separator = "to"),
),
mainPanel(
  #textOutput("rfratetext")
  #plotOutput("returns_hist"),
  plotOutput("correlation_plot_view"),
  #plotOutput("stock_price_history"),
  #plotOutput("efficency_frontier"),
  #plotOutput("compare_SP500")
  )
)

server <- function(input, output) {

output$correlation_plot_view <- renderPlot({
  
  risk_free_rate <- input$rfrate
  
  tickers <- input$tickers
  
  from_date <- input$fromdate
  to_date <- input$todate
  
  
  input <- stock_input(tickers,from_date ,to_date)
 
  correlation_plot(input[[4]])
})
  
}


shinyApp(ui = ui, server = server)