load_data_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Load Data",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          ns("data_source"),
          "Choose data source:",
          choices = c("Built-in dataset", "Upload file"),
          selected = "Built-in dataset"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Built-in dataset'", ns("data_source")),
          selectInput(
            ns("builtin_data"),
            "Select a built-in dataset:",
            choices = c("airbnb.csv", "dataset_Facebook.csv")
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Upload file'", ns("data_source")),
          fileInput(
            ns("file_upload"),
            "Upload a dataset",
            accept = c(".csv", ".xlsx", ".xls", ".json", ".rds")
          )
        )
      ),
      
      mainPanel(
        h3("Current Data Source"),
        verbatimTextOutput(ns("data_source_info")),
        br(),
        
        h3("Detected Delimiter"),
        verbatimTextOutput(ns("delimiter_info")),
        br(),
        
        h3("Dataset Information"),
        verbatimTextOutput(ns("data_dimensions")),
        br(),
        
        h3("Column Types"),
        DTOutput(ns("column_info_table")),
        br(),
        
        h3("Data Preview"),
        DTOutput(ns("preview_table"))
      )
    )
  )
}

load_data_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {
    
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
      file_path <- file.path("data", dataset_name)
      file_ext <- tolower(tools::file_ext(file_path))
      
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
        stop("Unsupported built-in dataset format.")
      }
    }
    
    current_data <- reactive({
      if (input$data_source == "Built-in dataset") {
        tryCatch({
          df <- read_builtin_data(input$builtin_data)
          raw_data(df)
          return(df)
        }, error = function(e) {
          showNotification(
            paste("Error reading built-in dataset:", e$message),
            type = "error"
          )
          raw_data(NULL)
          return(NULL)
        })
      } else {
        req(input$file_upload)
        
        file_path <- input$file_upload$datapath
        file_name <- input$file_upload$name
        file_ext <- tools::file_ext(file_name)
        
        tryCatch({
          df <- read_uploaded_data(file_path, file_ext)
          raw_data(df)
          return(df)
        }, error = function(e) {
          showNotification(
            paste("Error reading uploaded file:", e$message),
            type = "error"
          )
          raw_data(NULL)
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
            "Detected delimiter: comma (,)"
          } else if (delimiter == ";") {
            "Detected delimiter: semicolon (;)"
          } else if (delimiter == "\t") {
            "Detected delimiter: tab"
          } else {
            paste("Detected delimiter:", delimiter)
          }
        } else {
          "Delimiter detection only applies to CSV files."
        }
      } else {
        req(input$file_upload)
        file_name <- input$file_upload$name
        file_ext <- tolower(tools::file_ext(file_name))
        
        if (file_ext == "csv") {
          file_path <- input$file_upload$datapath
          delimiter <- detect_delimiter(file_path)
          
          if (delimiter == ",") {
            "Detected delimiter: comma (,)"
          } else if (delimiter == ";") {
            "Detected delimiter: semicolon (;)"
          } else if (delimiter == "\t") {
            "Detected delimiter: tab"
          } else {
            paste("Detected delimiter:", delimiter)
          }
        } else {
          "Delimiter detection only applies to CSV files."
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
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    output$preview_table <- renderDT({
      req(current_data())
      df <- current_data()
      datatable(
        head(df, 10),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
  })
}