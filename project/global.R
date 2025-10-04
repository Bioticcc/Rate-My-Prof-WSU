#FOR PUSHING TO THE GITHUB REPO, FOLLOW THESE STEPS:
#git status #whats changed
#git add . #all files or specific (specific is prolly safer)
#git commit -m "your message here" #commit plus message
#git push #pushes changes to repo!

#FOR PULLING FROM THE MAIN REPO, FOLLOW THESE STEPS:
#git status
#git pull origin main


#When running from VScode, this is how you do it:
#(Ctrl + ~) to open terminal for project.
#cd project (unless you are already in that folder)
#run the command: R (this will open R terminal similair to the ghci thing from 355)
#run the command: shiny::runApp()
#when you want to close the app, click on terminal and press: (Ctrl C)
#close the newly opened browser tab!

# -------- global.R --------

# General options (optional but handy)
options(shiny.autoreload = TRUE)
options(shiny.reactlog = FALSE)
# If you use bslib Google fonts:
# options(bslib.fonts.download = "yes")

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
# library(RPostgres)   # keep only if you actually use Postgres
library(pool)
library(dplyr)
library(dbplyr)

# Data wrangling & export
library(tidyr)
library(readr)
library(jsonlite)

# Plots (if you use them)
library(ggplot2)
library(plotly)

# ---- Globals / config / helpers go here ----
APP_TITLE <- "Rate-My-Prof-WSU"

# Example: paths, constants, small helper functions
data_dir <- "data"

# Example helper (available in both ui.R and server.R)
is_admin <- function(auth) isTRUE(auth$user_info$admin)