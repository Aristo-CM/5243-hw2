library(shiny)
library(DT)
library(readr)
library(readxl)
library(jsonlite)
library(stringr)

# ---------- Helper Functions ----------

detect_delimiter <- function(file_path) {
  first_line <- readLines(file_path, n = 1, warn = FALSE)
  
  comma_count <- stringr::str_count(first_line, ",")
  semicolon_count <- stringr::str_count(first_line, ";")
  tab_count <- stringr::str_count(first_line, "\t")
  
  counts <- c(comma = comma_count, semicolon = semicolon_count, tab = tab_count)
  best <- names(which.max(counts))
  
  if (best == "comma") return(",")
  if (best == "semicolon") return(";")
  if (best == "tab") return("\t")
  
  return(",")
}

read_uploaded_data <- function(file_path, file_ext) {
  file_ext <- tolower(file_ext)
  
  if (file_ext == "csv") {
    delimiter <- detect_delimiter(file_path)
    df <- readr::read_delim(
      file_path,
      delim = delimiter,
      show_col_types = FALSE
    )
    return(as.data.frame(df))
    
  } else if (file_ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(file_path)
    return(as.data.frame(df))
    
  } else if (file_ext == "json") {
    df <- jsonlite::fromJSON(file_path)
    return(as.data.frame(df))
    
  } else if (file_ext == "rds") {
    df <- readRDS(file_path)
    return(as.data.frame(df))
    
  } else {
    stop("Unsupported file format. Please upload CSV, Excel, JSON, or RDS.")
  }
}

get_column_info <- function(df) {
  data.frame(
    Column = names(df),
    Type = sapply(df, function(x) class(x)[1]),
    stringsAsFactors = FALSE
  )
}

read_builtin_data <- function(dataset_name) {
  if (dataset_name == "amazon.csv") {
    file_path <- "data/amazon.csv"
    delimiter <- detect_delimiter(file_path)
    df <- readr::read_delim(
      file_path,
      delim = delimiter,
      show_col_types = FALSE
    )
    return(as.data.frame(df))
    
  } else if (dataset_name == "dataset_Facebook.csv") {
    file_path <- "data/dataset_Facebook.csv"
    delimiter <- detect_delimiter(file_path)
    df <- readr::read_delim(
      file_path,
      delim = delimiter,
      show_col_types = FALSE
    )
    return(as.data.frame(df))
    
  } else {
    stop("Unknown built-in dataset.")
  }
}

# ---------- UI ----------

ui <- fluidPage(
  titlePanel("Data Analysis"),
  
  tabsetPanel(
    tabPanel(
      "Home",
      br(),
      h3("User Guide"),
      tags$ol(
        tags$li("Go to the Load Data tab."),
        tags$li("Choose either a built-in dataset or upload your own file."),
        tags$li("Built-in datasets are stored in the project data folder."),
        tags$li("Supported upload formats: CSV, Excel, JSON, and RDS."),
        tags$li("For CSV files, the app automatically detects the delimiter."),
        tags$li("Check the dataset source, dimensions, column types, and preview.")
      )
    ),
    
    tabPanel(
      "Load Data",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "data_source",
            "Choose data source:",
            choices = c("Built-in dataset", "Upload file"),
            selected = "Built-in dataset"
          ),
          
          conditionalPanel(
            condition = "input.data_source == 'Built-in dataset'",
            selectInput(
              "builtin_data",
              "Select a built-in dataset:",
              choices = c("amazon.csv", "dataset_Facebook.csv")
            )
          ),
          
          conditionalPanel(
            condition = "input.data_source == 'Upload file'",
            fileInput(
              "file_upload",
              "Upload a dataset",
              accept = c(".csv", ".xlsx", ".xls", ".json", ".rds")
            )
          )
        ),
        
        mainPanel(
          h3("Current Data Source"),
          verbatimTextOutput("data_source_info"),
          br(),
          
          h3("Detected Delimiter"),
          verbatimTextOutput("delimiter_info"),
          br(),
          
          h3("Dataset Information"),
          verbatimTextOutput("data_dimensions"),
          br(),
          
          h3("Column Types"),
          DTOutput("column_info_table"),
          br(),
          
          h3("Data Preview"),
          DTOutput("preview_table")
        )
      )
    ),
    
    tabPanel("Cleaning", "Coming soon"),
    tabPanel("Feature Engineering", "Coming soon"),
    tabPanel("EDA", "Coming soon")
  )
)

# ---------- Server ----------

server <- function(input, output, session) {
  
  current_data <- reactive({
    if (input$data_source == "Built-in dataset") {
      tryCatch({
        df <- read_builtin_data(input$builtin_data)
        return(df)
      }, error = function(e) {
        showNotification(
          paste("Error reading built-in dataset:", e$message),
          type = "error"
        )
        return(NULL)
      })
      
    } else {
      req(input$file_upload)
      
      file_path <- input$file_upload$datapath
      file_name <- input$file_upload$name
      file_ext <- tools::file_ext(file_name)
      
      tryCatch({
        df <- read_uploaded_data(file_path, file_ext)
        return(df)
      }, error = function(e) {
        showNotification(
          paste("Error reading uploaded file:", e$message),
          type = "error"
        )
        return(NULL)
      })
    }
  })
  
  output$data_source_info <- renderText({
    if (input$data_source == "Built-in dataset") {
      paste("Using built-in dataset:", input$builtin_data)
    } else {
      req(input$file_upload)
      paste("Uploaded file:", input$file_upload$name)
    }
  })
  
  output$delimiter_info <- renderText({
    if (input$data_source == "Built-in dataset") {
      req(input$builtin_data)
      file_path <- file.path("data", input$builtin_data)
      file_ext <- tolower(tools::file_ext(file_path))
      
      if (file_ext == "csv") {
        delimiter <- detect_delimiter(file_path)
        
        if (delimiter == ",") {
          return("Detected delimiter: comma (,)")
        } else if (delimiter == ";") {
          return("Detected delimiter: semicolon (;)")
        } else if (delimiter == "\t") {
          return("Detected delimiter: tab")
        } else {
          return(paste("Detected delimiter:", delimiter))
        }
      } else {
        return("Delimiter detection only applies to CSV files.")
      }
      
    } else {
      req(input$file_upload)
      file_name <- input$file_upload$name
      file_ext <- tolower(tools::file_ext(file_name))
      
      if (file_ext == "csv") {
        file_path <- input$file_upload$datapath
        delimiter <- detect_delimiter(file_path)
        
        if (delimiter == ",") {
          return("Detected delimiter: comma (,)")
        } else if (delimiter == ";") {
          return("Detected delimiter: semicolon (;)")
        } else if (delimiter == "\t") {
          return("Detected delimiter: tab")
        } else {
          return(paste("Detected delimiter:", delimiter))
        }
      } else {
        return("Delimiter detection only applies to CSV files.")
      }
    }
  })
  
  output$data_dimensions <- renderText({
    req(current_data())
    df <- current_data()
    paste("Rows:", nrow(df), "| Columns:", ncol(df))
  })
  
  output$column_info_table <- renderDT({
    req(current_data())
    df <- current_data()
    
    datatable(
      get_column_info(df),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  output$preview_table <- renderDT({
    req(current_data())
    df <- current_data()
    
    datatable(
      head(df, 10),
      options = list(
        pageLength = 10, 
        scrollX = TRUE
      )
    )
  })
}

shinyApp(ui = ui, server = server)