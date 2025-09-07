library(shiny)

# Load ui and server definitions from separate files
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)