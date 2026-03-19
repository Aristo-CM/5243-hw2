cleaning_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Cleaning",
    h3("Cleaning & Preprocessing"),
    p("This section will be implemented by Person 2."),
    p("Suggested tasks: duplicates, missing values, scaling, encoding, outlier handling.")
  )
}

cleaning_server <- function(id, raw_data, cleaned_data) {
  moduleServer(id, function(input, output, session) {
    observe({
      if (!is.null(raw_data())) {
        cleaned_data(raw_data())
      }
    })
  })
}