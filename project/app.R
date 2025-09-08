#FOR PUSHING TO THE GITHUB REPO, FOLLOW THESE STEPS:
#git status #whats changed
#git add . #all files or specific (specific is prolly safer)
#git commit -m "your message here" #commit plus message
#git push #pushes changes to repo!

#When running from VScode, this is how you do it:
#(Ctrl + ~) to open terminal for project.
#cd project (unless you are already in that folder)
#run the command: R (this will open R terminal similair to the ghci thing from 355)
#run the command: shiny::runApp()
#when you want to close the app, click on terminal and press: (Ctrl C)
#close the newly opened browser tab!


# Core
library(shiny)

# UI polish / widgets
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(shinyvalidate)
library(DT)

# Authentication
library(shinymanager)  
       
# Data / persistence
library(DBI)
library(RSQLite)      
library(RPostgres)    
library(pool)
library(dplyr)
library(dbplyr)

# Data wrangling & export
library(tidyr)
library(readr)
library(jsonlite)

# Plots (optional)
library(ggplot2)
library(plotly)

# Load ui and server definitions from separate files
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)