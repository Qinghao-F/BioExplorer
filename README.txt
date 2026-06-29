To launch the app, 
open RStudio (or any R session), 
set the working directory to the project root, 
and run 'shiny::runApp("app.R")' or equivalently 'source("app.R")'. 

The needed libraries are listed in    global.R:
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(readr)
library(stringr)
library(htmltools)
library(tibble)
library(leaflet)
library(leaflet.extras)
library(ggplot2)

Shiny will automatically load the prebuilt static assets from the directory, and the application will start normally.
