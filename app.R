# Master Shiny ML App
# Shelli Kesler 4/22/25

options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB

library(factoextra)
library(shiny)
library(cluster)
library(dplyr)
library(shinyFiles)
library(ggplot2)
library(fs)
library(DT)
library(markdown)
library(naniar)
library(missRanger)
library(readr)
library(gridExtra)
library(rlang)
library(randomForest)
library(caret)
library(pROC)
library(shinyjs)
library(dplyr)
library(tidyr)

# Install and load required packages
required_packages <- c(
"shiny", "cluster", "factoextra", "dplyr", "shinyFiles", "ggplot2", "fs",
   "DT", "markdown", "naniar", "missRanger", "readr", "gridExtra", "rlang",
   "randomForest", "caret", "pROC", "shinyjs"
 )

 new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
 if (length(new_packages)) install.packages(new_packages)

 invisible(lapply(required_packages, library, character.only = TRUE))

source("shinyMiss_v3.R")
source("shinyK_v2.R")
source("shinyRF_v4.R")

miss_app <- make_miss_app()
clust_app <- make_clust_app()
rf_app <- make_rf_app()

ui <- navbarPage("Sandbox ML",
                 tabPanel("Missing Data", miss_app$ui),
                 tabPanel("Clustering", clust_app$ui),
                 tabPanel("Random Forest", rf_app$ui),
)

server <- function(input, output, session) {
  miss_app$server(input, output, session)
  clust_app$server(input, output, session)
  rf_app$server(input, output, session)
}

shinyApp(ui, server)