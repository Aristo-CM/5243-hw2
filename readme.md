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

- Zhengxuan Xiao: Data Cleaning Module 

- Justine Dugger-Ades
  - Designed and implemented the Feature Engineering Module
  - Enabled arithmetic combinations of dataset features
  - Created proper response and status texts for feature engineering
  - Developed multiple methods of singular feature engineering

- Member 4: EDA Module  
