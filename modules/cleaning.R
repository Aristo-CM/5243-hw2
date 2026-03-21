library(shiny)
library(DT)
library(stringr)

# The Cleaning module is responsible for basic data preprocessing before feature engineering and exploratory analysis.
# It allows users to clean and standardize the dataset through an interactive interface. The module supports duplicate removal, missing value handling, numeric scaling, and outlier removal. 
# After the selected cleaning steps are applied, the processed dataset is stored and displayed for further analysis in the downstream modules.

# get mode for categorical columns
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

cleaning_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Cleaning",
    fluidPage(
      h3("Cleaning & Preprocessing"),
      p("This module allows users to remove duplicates, handle missing values, scale numeric columns, and remove outliers."),
      
      sidebarLayout(
        sidebarPanel(
          checkboxInput(ns("remove_duplicates"), "Remove duplicate rows", TRUE),
          
          selectInput(
            ns("missing_method"),
            "Handle Missing Values",
            choices = c(
              "Do Nothing" = "none",
              "Remove Rows with Missing Values" = "remove",
              "Impute Missing Values" = "impute"
            ),
            selected = "impute"
          ),
          
          checkboxInput(ns("scale_numeric"), "Scale Numeric Columns (Z-score)", FALSE),
          checkboxInput(ns("remove_outliers"), "Remove Outliers (IQR)", FALSE),
          
          actionButton(ns("apply_cleaning"), "Apply Cleaning")
        ),
        
        mainPanel(
          h4("Cleaning Summary"),
          verbatimTextOutput(ns("summary")),
          hr(),
          h4("Cleaned Data Preview"),
          DTOutput(ns("cleaned_table"))
        )
      )
    )
  )
}

cleaning_server <- function(id, raw_data, cleaned_data) {
  moduleServer(id, function(input, output, session) {
    
    summary_text <- reactiveVal("No cleaning has been applied yet.")
    
    observe({
      if (!is.null(raw_data()) && is.null(cleaned_data())) {
        cleaned_data(raw_data())
      }
    })
    
    observeEvent(input$apply_cleaning, {
      req(raw_data())
      df <- raw_data()
      
      original_rows <- nrow(df)
      original_cols <- ncol(df)
      summary_lines <- c()
      
      for (col in names(df)) {
        if (is.character(df[[col]])) {
          df[[col]] <- str_trim(df[[col]])
          df[[col]][df[[col]] == ""] <- NA
        }
      }
      summary_lines <- c(summary_lines, "Standardized text columns by trimming spaces and converting empty strings to NA.")
      
      if (input$remove_duplicates) {
        before <- nrow(df)
        df <- df[!duplicated(df), , drop = FALSE]
        removed <- before - nrow(df)
        summary_lines <- c(summary_lines, paste("Removed duplicate rows:", removed))
      }
      
      if (input$missing_method == "remove") {
        before <- nrow(df)
        df <- na.omit(df)
        removed <- before - nrow(df)
        summary_lines <- c(summary_lines, paste("Removed rows with missing values:", removed))
        
      } else if (input$missing_method == "impute") {
        for (col in names(df)) {
          if (any(is.na(df[[col]]))) {
            if (is.numeric(df[[col]])) {
              df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
            } else {
              df[[col]][is.na(df[[col]])] <- get_mode(df[[col]])
            }
          }
        }
        summary_lines <- c(summary_lines, "Imputed missing values using mean for numeric columns and mode for categorical columns.")
      }
      
      if (input$scale_numeric) {
        numeric_cols <- sapply(df, is.numeric)
        df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
          if (sd(x, na.rm = TRUE) == 0 || all(is.na(x))) return(x)
          as.numeric(scale(x))
        })
        summary_lines <- c(summary_lines, "Scaled numeric columns using Z-score standardization.")
      }
      
      if (input$remove_outliers) {
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        
        if (length(numeric_cols) > 0) {
          keep <- rep(TRUE, nrow(df))
          
          for (col in numeric_cols) {
            q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
            q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
            iqr_val <- q3 - q1
            lower <- q1 - 1.5 * iqr_val
            upper <- q3 + 1.5 * iqr_val
            
            keep <- keep & (is.na(df[[col]]) | (df[[col]] >= lower & df[[col]] <= upper))
          }
          
          before <- nrow(df)
          df <- df[keep, , drop = FALSE]
          removed <- before - nrow(df)
          summary_lines <- c(summary_lines, paste("Removed outlier rows:", removed))
        }
      }
      
      cleaned_data(df)
      
      summary_lines <- c(
        summary_lines,
        paste("Original dataset size:", original_rows, "rows x", original_cols, "columns"),
        paste("Final dataset size:", nrow(df), "rows x", ncol(df), "columns")
      )
      
      summary_text(paste(summary_lines, collapse = "\n"))
    })
    
    output$summary <- renderText({
      summary_text()
    })
    
    output$cleaned_table <- renderDT({
      req(cleaned_data())
      datatable(
        cleaned_data(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}

