library(shiny)
library(leaflet)
library(DT)

library(readr)

FIRST_YEAR <- 2021
LAST_YEAR <- 2023

ranks <- read.csv("data/ranks.csv")

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)