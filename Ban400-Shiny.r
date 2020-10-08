

######## RUN THIS IN CONSOLE #########

library(shiny)

ui <- fluidPage("Hello world")  # create the UI in the APP

server <- function(input, output) {}  # Make the server functions

shinyApp(ui = ui, server = server) # Combine it into the app

########################################



