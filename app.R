library(shiny)
library(DT)
library(readr)
library(readxl)
library(jsonlite)
library(stringr)

source("modules/load_data.R")
source("modules/cleaning.R")
source("modules/feature.R")
source("modules/eda.R")

ui <- fluidPage(
  titlePanel("Data Analysis"),
  
  tabsetPanel(
    load_data_ui("load"),
    cleaning_ui("clean"),
    feature_ui("feature"),
    eda_ui("eda")
  )
)

server <- function(input, output, session) {
  raw_data <- reactiveVal(NULL)
  cleaned_data <- reactiveVal(NULL)
  featured_data <- reactiveVal(NULL)
  
  load_data_server("load", raw_data)
  cleaning_server("clean", raw_data, cleaned_data)
  feature_server("feature", cleaned_data, featured_data)
  eda_server("eda", featured_data)
}

shinyApp(ui = ui, server = server)