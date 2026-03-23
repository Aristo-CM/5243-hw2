library(shiny)
library(DT)
library(stringr)

feature_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Feature Engineering",
    fluidPage(
      h3("Feature Engineering"),
      p("This section allows users to create new columns through transformations of existing columns."),
      
      sidebarLayout(
        sidebarPanel(
          uiOutput(ns("var_select")),
          radioButtons(
            ns("transformation"),
            "Choose Transformation:",
            choices = c("None", "Log-transformation", "Normalizing", "Standardizing")
          ),
          actionButton(ns("apply_featured"), "Apply Transformation")
        ),
        mainPanel(
          h4("Feature Engineering Summary"),
          verbatimTextOutput(ns("summary")),
          hr(),
          h4("Data Preview with New Features"),
          DTOutput(ns("featured_table"))
        )
      )
    )
  )
}

feature_server <- function(id, cleaned_data, featured_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    summary_text <- reactiveVal("No feature engineering has been applied yet.")
    
    output$var_select <- renderUI({
      req(cleaned_data())
      cols <- names(cleaned_data())
      selectInput(ns("variable"), "Select Variable:", choices = cols)
    })
    
    observe({
      if (!is.null(cleaned_data()) && is.null(featured_data())) {
        featured_data(cleaned_data())
      }
    })
    
    observeEvent(input$apply_featured, {
      req(input$variable, input$transformation, cleaned_data())
      
      df <- cleaned_data()
      temp <- df[[input$variable]]
      
      if (!is.numeric(temp)) {
        summary_text("Please select a numeric variable.")
        return(NULL)
      }
      
      if (input$transformation == "None") {
        featured_data(df)
        summary_text("No transformation selected.")
        return(NULL)
      }
      
      original_rows <- nrow(df)
      original_cols <- ncol(df)
      summary_lines <- c()
      
      if (input$transformation == "Log-transformation") {
        df[[paste0(input$variable, ".log")]] <- log(temp + 1)
        summary_lines <- c(summary_lines, "Created a new feature using log transformation.")
        
      } else if (input$transformation == "Normalizing") {
        df[[paste0(input$variable, ".norm")]] <- (temp - min(temp, na.rm = TRUE)) / 
          (max(temp, na.rm = TRUE) - min(temp, na.rm = TRUE))
        summary_lines <- c(summary_lines, "Created a new feature using normalization.")
        
      } else if (input$transformation == "Standardizing") {
        df[[paste0(input$variable, ".standard")]] <- as.numeric(scale(temp))
        summary_lines <- c(summary_lines, "Created a new feature using standardization.")
      }
      
      featured_data(df)
      
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
    
    output$featured_table <- renderDT({
      req(featured_data())
      datatable(
        featured_data(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}