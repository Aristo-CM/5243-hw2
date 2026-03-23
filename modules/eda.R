library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(rlang)

eda_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "EDA",
    fluidPage(
      h3("Exploratory Data Analysis"),
      
      wellPanel(
        tags$b("How to use this page"),
        tags$ol(
          tags$li("Optionally filter the dataset first."),
          tags$li("Choose a plot type and variables."),
          tags$li("Review summary statistics, correlation, and preview table.")
        )
      ),
      
      fluidRow(
        column(
          width = 3,
          h4("Controls"),
          
          uiOutput(ns("filter_col_ui")),
          uiOutput(ns("filter_value_ui")),
          tags$hr(),
          
          selectInput(
            ns("plot_type"),
            "Plot Type",
            choices = c("Histogram", "Scatter", "Boxplot", "Bar Chart"),
            selected = "Histogram"
          ),
          
          uiOutput(ns("x_var_ui")),
          uiOutput(ns("y_var_ui")),
          uiOutput(ns("color_var_ui")),
          uiOutput(ns("bins_ui"))
        ),
        
        column(
          width = 9,
          tabsetPanel(
            tabPanel(
              "Overview",
              br(),
              verbatimTextOutput(ns("dataset_info")),
              br(),
              h4("Numeric Summary"),
              DTOutput(ns("summary_table"))
            ),
            tabPanel(
              "Visualization",
              br(),
              plotlyOutput(ns("eda_plot"), height = "520px")
            ),
            tabPanel(
              "Correlation",
              br(),
              plotlyOutput(ns("corr_plot"), height = "520px")
            ),
            tabPanel(
              "Preview",
              br(),
              DTOutput(ns("filtered_table"))
            )
          )
        )
      )
    )
  )
}

eda_server <- function(id, featured_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    safe_stat <- function(x, fun) {
      if (all(is.na(x))) return(NA_real_)
      fun(x, na.rm = TRUE)
    }
    
    current_data <- reactive({
      req(featured_data())
      as.data.frame(featured_data(), check.names = FALSE)
    })
    
    numeric_cols <- reactive({
      df <- current_data()
      names(df)[vapply(df, is.numeric, logical(1))]
    })
    
    categorical_cols <- reactive({
      df <- current_data()
      names(df)[vapply(
        df,
        function(x) is.character(x) || is.factor(x) || is.logical(x),
        logical(1)
      )]
    })
    
    output$filter_col_ui <- renderUI({
      req(current_data())
      
      selectInput(
        ns("filter_col"),
        "Filter Column",
        choices = c("None", names(current_data())),
        selected = "None"
      )
    })
    
    output$filter_value_ui <- renderUI({
      req(current_data(), input$filter_col)
      
      if (input$filter_col == "None") {
        return(NULL)
      }
      
      df <- current_data()
      x <- df[[input$filter_col]]
      
      if (is.numeric(x)) {
        rng <- range(x, na.rm = TRUE)
        
        if (!all(is.finite(rng))) {
          return(helpText("This numeric column only contains missing values."))
        }
        
        sliderInput(
          ns("filter_num"),
          "Value Range",
          min = rng[1],
          max = rng[2],
          value = rng
        )
      } else {
        vals <- sort(unique(as.character(x)))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        
        if (length(vals) == 0) {
          return(helpText("This categorical column has no valid values."))
        }
        
        selectInput(
          ns("filter_cat"),
          "Values",
          choices = vals,
          selected = vals,
          multiple = TRUE
        )
      }
    })
    
    filtered_data <- reactive({
      df <- current_data()
      
      if (is.null(input$filter_col) || input$filter_col == "None") {
        return(df)
      }
      
      x <- df[[input$filter_col]]
      
      if (is.numeric(x)) {
        req(input$filter_num)
        keep <- !is.na(x) & x >= input$filter_num[1] & x <= input$filter_num[2]
        df[keep, , drop = FALSE]
      } else {
        if (is.null(input$filter_cat) || length(input$filter_cat) == 0) {
          return(df[0, , drop = FALSE])
        }
        keep <- !is.na(x) & as.character(x) %in% input$filter_cat
        df[keep, , drop = FALSE]
      }
    })
    
    output$dataset_info <- renderText({
      df_all <- current_data()
      df <- filtered_data()
      
      paste(
        paste("Rows (filtered):", nrow(df)),
        paste("Columns:", ncol(df)),
        paste("Original rows:", nrow(df_all)),
        paste("Numeric columns:", length(names(df)[vapply(df, is.numeric, logical(1))])),
        paste(
          "Categorical/logical columns:",
          length(names(df)[vapply(
            df,
            function(x) is.character(x) || is.factor(x) || is.logical(x),
            logical(1)
          )])
        ),
        paste("Missing cells in filtered data:", sum(is.na(df))),
        sep = "\n"
      )
    })
    
    summary_stats <- reactive({
      df <- filtered_data()
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      
      if (length(num_cols) == 0) {
        return(NULL)
      }
      
      data.frame(
        Variable = num_cols,
        Missing = sapply(df[num_cols], function(x) sum(is.na(x))),
        Mean = round(sapply(df[num_cols], function(x) safe_stat(x, mean)), 4),
        Median = round(sapply(df[num_cols], function(x) safe_stat(x, median)), 4),
        SD = round(sapply(df[num_cols], function(x) safe_stat(x, sd)), 4),
        Min = round(sapply(df[num_cols], function(x) safe_stat(x, min)), 4),
        Max = round(sapply(df[num_cols], function(x) safe_stat(x, max)), 4),
        row.names = NULL
      )
    })
    
    output$summary_table <- renderDT({
      stats <- summary_stats()
      
      shiny::validate(
        shiny::need(!is.null(stats), "No numeric columns available for summary statistics.")
      )
      
      datatable(
        stats,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    output$x_var_ui <- renderUI({
      req(filtered_data(), input$plot_type)
      
      if (input$plot_type == "Histogram") {
        cols <- numeric_cols()
        if (length(cols) == 0) return(helpText("No numeric columns available."))
        selectInput(ns("x_var"), "X Variable", choices = cols, selected = cols[1])
        
      } else if (input$plot_type == "Scatter") {
        cols <- numeric_cols()
        if (length(cols) < 2) return(helpText("Need at least two numeric columns."))
        selectInput(ns("x_var"), "X Variable", choices = cols, selected = cols[1])
        
      } else if (input$plot_type == "Boxplot") {
        group_cols <- categorical_cols()
        selectInput(
          ns("x_var"),
          "Group Variable (optional)",
          choices = c("None", group_cols),
          selected = "None"
        )
        
      } else if (input$plot_type == "Bar Chart") {
        cols <- categorical_cols()
        if (length(cols) == 0) return(helpText("No categorical columns available."))
        selectInput(ns("x_var"), "Category Variable", choices = cols, selected = cols[1])
      }
    })
    
    output$y_var_ui <- renderUI({
      req(filtered_data(), input$plot_type)
      
      if (input$plot_type %in% c("Scatter", "Boxplot")) {
        cols <- numeric_cols()
        if (length(cols) == 0) return(helpText("No numeric columns available."))
        selectInput(
          ns("y_var"),
          "Y Variable",
          choices = cols,
          selected = cols[min(2, length(cols))]
        )
      } else {
        NULL
      }
    })
    
    output$color_var_ui <- renderUI({
      req(filtered_data(), input$plot_type)
      
      if (input$plot_type == "Scatter") {
        selectInput(
          ns("color_var"),
          "Color By",
          choices = c("None", names(filtered_data())),
          selected = "None"
        )
      } else {
        NULL
      }
    })
    
    output$bins_ui <- renderUI({
      req(input$plot_type)
      
      if (input$plot_type == "Histogram") {
        sliderInput(ns("bins"), "Number of Bins", min = 5, max = 50, value = 20)
      } else {
        NULL
      }
    })
    
    output$eda_plot <- renderPlotly({
      df <- filtered_data()
      
      shiny::validate(
        shiny::need(nrow(df) > 0, "No rows remain after filtering.")
      )
      
      if (input$plot_type == "Histogram") {
        req(input$x_var, input$bins)
        
        p <- ggplot(df, aes(x = .data[[input$x_var]])) +
          geom_histogram(bins = input$bins, fill = "#2C7FB8", color = "white") +
          theme_minimal() +
          labs(
            title = paste("Histogram of", input$x_var),
            x = input$x_var,
            y = "Count"
          )
        
      } else if (input$plot_type == "Scatter") {
        req(input$x_var, input$y_var)
        
        if (!is.null(input$color_var) && input$color_var != "None") {
          p <- ggplot(
            df,
            aes(
              x = .data[[input$x_var]],
              y = .data[[input$y_var]],
              color = .data[[input$color_var]]
            )
          ) +
            geom_point(alpha = 0.75, size = 2) +
            theme_minimal() +
            labs(
              title = paste(input$y_var, "vs", input$x_var),
              x = input$x_var,
              y = input$y_var,
              color = input$color_var
            )
        } else {
          p <- ggplot(
            df,
            aes(
              x = .data[[input$x_var]],
              y = .data[[input$y_var]]
            )
          ) +
            geom_point(alpha = 0.75, size = 2, color = "#2C7FB8") +
            theme_minimal() +
            labs(
              title = paste(input$y_var, "vs", input$x_var),
              x = input$x_var,
              y = input$y_var
            )
        }
        
      } else if (input$plot_type == "Boxplot") {
        req(input$y_var)
        
        if (!is.null(input$x_var) && input$x_var != "None") {
          p <- ggplot(
            df,
            aes(
              x = .data[[input$x_var]],
              y = .data[[input$y_var]]
            )
          ) +
            geom_boxplot(fill = "#74A9CF") +
            theme_minimal() +
            labs(
              title = paste("Boxplot of", input$y_var, "by", input$x_var),
              x = input$x_var,
              y = input$y_var
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
          p <- ggplot(df, aes(y = .data[[input$y_var]])) +
            geom_boxplot(fill = "#74A9CF") +
            theme_minimal() +
            labs(
              title = paste("Boxplot of", input$y_var),
              y = input$y_var
            )
        }
        
      } else if (input$plot_type == "Bar Chart") {
        req(input$x_var)
        
        p <- ggplot(df, aes(x = .data[[input$x_var]])) +
          geom_bar(fill = "#41AB5D") +
          theme_minimal() +
          labs(
            title = paste("Count of", input$x_var),
            x = input$x_var,
            y = "Count"
          ) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      ggplotly(p)
    })
    
    output$corr_plot <- renderPlotly({
      df <- filtered_data()
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      
      shiny::validate(
        shiny::need(length(num_cols) >= 2, "Need at least two numeric columns for correlation.")
      )
      
      cor_mat <- cor(df[num_cols], use = "pairwise.complete.obs")
      
      plot_ly(
        x = colnames(cor_mat),
        y = rownames(cor_mat),
        z = cor_mat,
        type = "heatmap",
        colorscale = "RdBu",
        zmin = -1,
        zmax = 1
      ) %>%
        layout(
          title = "Correlation Matrix",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
    
    output$filtered_table <- renderDT({
      df <- filtered_data()
      
      datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}

