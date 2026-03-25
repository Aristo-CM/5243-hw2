# Data Analysis App (Project 2)

This project is a web-based data analysis application developed using R Shiny. It provides an interactive interface for users to load, clean, transform, and explore datasets.

## Deployed Application

https://w-analytics.shinyapps.io/5243-hw2/

---

## Project Structure
- app.R  
- data/  
  - amazon.csv  
  - dataset_Facebook.csv  
- modules/  
  - load_data.R  
  - cleaning.R  
  - feature.R  
  - eda.R  
- README.md  

---

## How to Run the code

### 1. Install Required Packages

Run the following in R:

install.packages(c(
  "shiny",
  "DT",
  "readr",
  "readxl",
  "jsonlite",
  "stringr",
  "ggplot2",
  "plotly"
), repos = "https://cloud.r-project.org")

---

### 2. Run the Application

In the R console:

shiny::runApp()

---

## Features

- Load data from built-in datasets or upload files  
- Automatic delimiter detection  
- Display dataset information (rows, columns, types)  
- Data preview using interactive tables  
- Data cleaning (handling missing values, duplicates)  
- Feature engineering  
- Exploratory data analysis (EDA)  

---

## Team Contributions

- Jiahao Wang  
  - Designed and implemented the Data Loading Module  
  - Built the Shiny app structure and modular framework  
  - Implemented automatic delimiter detection  
  - Integrated built-in and uploaded dataset functionality  
  - Deployed the application to shinyapps.io  

- Zhengxuan Xiao
  - Designed and implemented the data cleaning module
  - Developed key preprocessing functionalities such as duplicate removal, missing value handling, normalization, and outlier detection.
  - Created a dynamic cleaning summary and data preview interface to help users understand the impact of each operation.

- Justine Dugger-Ades
  - Designed and implemented the Feature Engineering Module
  - Enabled arithmetic combinations of dataset features
  - Created proper response and status texts for feature engineering
  - Developed multiple methods of singular feature engineering

- Kuangda Qu
  - Designed and implemented the EDA module of the Shiny application
  - Built interactive visualizations, including histogram, scatter plot, boxplot, and bar chart
  - Implemented dynamic filtering for both numeric and categorical variables
  - Improved the overall UI/UX of the EDA page by organizing controls and outputs into a clear, responsive layout
  - Integrate the EDA module into the shared reactive data pipeline so that it works with the outputs from the loading, cleaning, and feature engineering modules
  - Contributed to the final report writing and compilation


