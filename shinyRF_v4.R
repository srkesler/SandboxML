# Shiny RF app
make_rf_app <- function(){
rf_ui <- tagList(
  useShinyjs(),
  titlePanel("Random Forest Modeling"),
  
    sidebarLayout(
      sidebarPanel(
        helpText(tags$em("File should contain only the variables selected for modeling and have no missing data.")),
        fileInput("rf_datafile", "Upload CSV File", accept = ".csv"),
        uiOutput("rf_outcome_select"),

        radioButtons("rf_type", "Model Type:",
                     choices = c("Classification", "Regression")),

        radioButtons("cv_method", "Cross-Validation Method:",
                     choices = c("None", "K-folds", "LOOCV"), selected = "None"),
        
        conditionalPanel(
          condition = "input.rf_type == 'Classification'",
          radioButtons("class_balance", "Class Balancing Method:",
                       choices = c("None", "Upsample", "Downsample"), selected = "None")
        ),

        conditionalPanel(
          condition = "input.cv_method == 'K-folds'",
          numericInput("k_folds", "Number of folds:", value = 10, min = 2)
        ),
        
        radioButtons("param_mode", "Parameter Selection:",
                     choices = c("Manual", "Auto"), selected = "Auto"),
        
        conditionalPanel(
          condition = "input.param_mode == 'Manual'",
          numericInput("rf_mtry", "Number of variables at each split (mtry):", value = 2, min = 1),
          numericInput("rf_ntree", "Number of trees:", value = 100, min = 1),
          selectInput("rf_maxnodes",
                      "Maximum tree depth (maxnodes):",
                      choices = c("Maximum" = "NULL", 5, 10, 15),
                      selected = "NULL")
        ),

        checkboxInput("rf_scale_data", "Standardize the data?", FALSE),

        actionButton("run_rf", "Run Random Forest"), 
        
        tags$hr(),
        tags$div(
          style = "text-align: center;",
          tags$img(src = "sb_icon.png", width = "80%", height = "auto"),
          tags$br(),
          tags$hr()
        ),
        helpText(tags$em("\u00A9 Shelli Kesler 2025")),
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Results", 
                   verbatimTextOutput("rf_summary"),
                   verbatimTextOutput("rf_metrics"),
                   #verbatimTextOutput("rf_varimp_cutoff"),  
                   #tableOutput("rf_varimp"),
                   downloadButton("rf_save_model", "Download Model")
          ),

          tabPanel("Plots",
                  tags$p("Tip: right click on a plot to save it.", 
                        style = "color: gray; font-style: italic; margin-top: 10px;"),
                  plotOutput("rf_plot", height = "400px")
          ),

          tabPanel("Data",
                   h4("Uploaded Dataset"),
                   DT::dataTableOutput("rf_dataView"),
                   br(),
                   h4("Outcome Summary"),
                   verbatimTextOutput("outcome_summary")
          ),

        tabPanel("Code",
                h4("Random Forest Code"),
                verbatimTextOutput("rf_codeOutput")
        ),

          tabPanel("Predict",
            fileInput("rf_model_file", "Upload Saved Model (.RData)"),
            fileInput("rf_newdata", "Upload New Data (.csv)"),
            actionButton("rf_predict_btn", "Run Prediction"),
            DTOutput("rf_predictions"),
            downloadButton("rf_download_pred", "Download Predictions")
        ),

           tabPanel("Help",
                 h4(""),
                 uiOutput("rf_helpOutput")
          )
        )
      )
    )
  )

# Server 
rf_server <- function(input, output, session) {
  
  volumes <- c(Home = fs::path_home(), getVolumes()())
  shinyDirChoose(input, "rf_save_dir", roots = volumes, session = session)
  
  rf_save_path <- reactive({
    if (is.null(input$rf_save_dir)) return(NULL)
    parseDirPath(volumes, input$rf_save_dir)
  })

  rf_data <- reactive({
    req(input$rf_datafile)
    read_csv(input$rf_datafile$datapath,show_col_types = FALSE)
  })

  output$rf_outcome_select <- renderUI({
    req(rf_data())
    selectInput("rf_outcome", "Select Outcome Variable:", choices = names(rf_data()))
  })
  
  output$rf_dataView <- DT::renderDataTable({
    req(input$rf_datafile)
    df <- read.csv(input$rf_datafile$datapath)
    df
  })
  
  predictions <- reactiveVal(NULL)
  observations <- reactiveVal(NULL)
  probabilities <- reactiveVal(NULL)
  rocplot <- reactiveVal(NULL)
  
  observe({
    if (input$cv_method == "LOOCV") {
      updateRadioButtons(session, "class_balance", selected = "None")
      shinyjs::disable("class_balance")
    } else {
      shinyjs::enable("class_balance")
    }
  })

  observeEvent(input$run_rf, {
    set.seed(42)

    withProgress(message = "Training Random Forest model...", value = 0, {
    df <- rf_data()
    outcome <- input$rf_outcome
    predictors <- setdiff(names(df), outcome)
    
    incProgress(0.2)

    # Convert all binary variables to factors
    is_binary <- function(x) length(unique(na.omit(x))) == 2
    binary_vars <- sapply(df, is_binary)
    df[, binary_vars] <- lapply(df[, binary_vars, drop = FALSE], as.factor)
    
      if (input$rf_type == "Classification") {
      df[[outcome]] <- as.factor(df[[outcome]])
      
      if (all(levels(df[[outcome]]) %in% c("0", "1"))) {
        levels(df[[outcome]]) <- c("Class0", "Class1")
      }
    }

    if (input$rf_scale_data) {
      num_vars <- sapply(df[, predictors], is.numeric)
      df[, predictors[num_vars]] <- scale(df[, predictors[num_vars]])
    }

    if (input$rf_type == "Classification" && input$class_balance == "Upsample") {
  class_table <- table(df[[outcome]])
  minority_class <- names(which.min(class_table))
  minority_rows <- df[df[[outcome]] == minority_class, ]
  majority_rows <- df[df[[outcome]] != minority_class, ]
  
  # Double the minority class (sample with replacement)
  upsampled_minority <- minority_rows[sample(nrow(minority_rows), 
                                              nrow(minority_rows), 
                                              replace = TRUE), ]
  
  df <- rbind(minority_rows, upsampled_minority, majority_rows)
} else if (input$rf_type == "Classification" && input$class_balance == "Downsample") {
  outcome <- input$rf_outcome
  x_data <- df[, setdiff(names(df), outcome), drop = FALSE]
  y_data <- df[[outcome]]
  
  down_df <- downSample(x = x_data, y = y_data, yname = outcome, list = FALSE)
  df <- down_df
    }

    df <- df[sample(nrow(df)), ]
    
    

    form <- as.formula(paste(outcome, "~ ."))

    if (input$param_mode == "Auto") {
      p <- ncol(df) - 1
      mtry <- round(sqrt(p))
      ntree <- if (nrow(df) < 1000) 100 else 500
      maxnodes <- if (nrow(df) < 1000) NULL else 10
    } else {
      mtry <- input$rf_mtry
      ntree <- input$rf_ntree
      maxnodes <- if (input$rf_maxnodes == "NULL") NULL else as.numeric(input$rf_maxnodes)
    }
    
    incProgress(0.3)
    
    ctrl <- if (input$cv_method == "None") {
      trainControl(method = "none", classProbs = TRUE, savePredictions = TRUE)
    } else if (input$cv_method == "LOOCV") {
      trainControl(method = "LOOCV", classProbs = TRUE, savePredictions = TRUE)
    } else {
      trainControl(method = "cv", number = input$k_folds, classProbs = TRUE, savePredictions = TRUE)
    }

    rf_model <- train(
      form,
      data = df,
      method = "rf",
      trControl = ctrl,
      tuneGrid = data.frame(mtry = mtry),
      ntree = ntree,
      maxnodes = maxnodes
    )

    incProgress(1, detail = "Done!")
    })
    
    debug_model <<- rf_model
    
    output$rf_summary <- renderPrint({
      cat("Number of samples:",nrow(df), "\n")
      cat("Number of predictors:", ncol(df)-1, "\n")
      cat("Outcome variable:", input$rf_outcome, "\n")
      cat("\nmtry:", mtry)
      cat("\nnum trees:", ntree)
      cat("\ntree depth:", ifelse(is.null(maxnodes), "maximum", maxnodes), "\n")
    })

    output$rf_metrics <- renderPrint({
      preds <- rf_model$pred
      obs <- preds$obs
      
      if (input$rf_type == "Classification") {
        
        if (input$cv_method == "None"){
          # Use training data for predictions
          preds <- predict(rf_model, newdata = rf_model$trainingData)
          probs <- predict(rf_model, newdata = rf_model$trainingData, type = "prob")[["Class1"]]
          obs <- rf_model$trainingData$.outcome
          
          cm <- confusionMatrix(preds, obs, positive = "Class1")
          roc_obj <- roc(response = obs, predictor = probs, quiet = TRUE)
          auc_val <- auc(roc_obj)
          probabilities(probs)
          
          cat("Final class distribution for outcome variable:\n")
          print(table(obs))
          cat("\n")
          
          cat("AUC:", round(auc_val, 3), "\n")
          cat("F1 Score:", round(cm$byClass["F1"], 3), "\n")
          cat("Sensitivity:", round(cm$byClass["Sensitivity"], 3), "\n")
          cat("Specificity:", round(cm$byClass["Specificity"], 3), "\n")
          cat("\nConfusion Matrix:\n")
          print(cm$table)
        }
        else{
          preds <- rf_model$pred
          obs <- preds$obs
          pred_class <- preds$pred
          cm <- confusionMatrix(preds$pred, obs, positive = levels(obs)[2])
          roc_obj <- roc(response = obs, predictor = preds$Class1, quiet = TRUE)
          auc_val <- auc(roc_obj)
          
          # Suppress class distribution if LOOCV (not meaningful)
          if (input$cv_method != "LOOCV") {
            cat("Final class distribution for outcome variable:\n")
            print(table(obs))
            cat("\n")
            cat("F1 Score:", round(cm$byClass["F1"], 3), "\n")
          }
        
          cat("AUC:", round(auc_val, 3), "\n")
          cat("Sensitivity:", round(cm$byClass["Sensitivity"], 3), "\n")
          cat("Specificity:", round(cm$byClass["Specificity"], 3), "\n")
          cat("\nConfusion Matrix:\n")
          print(cm$table)
        }
        
        rocplot(roc_obj)
        
      } else {  # regression block
        if (input$cv_method == "None") {
          # Use training data for predictions
          pred_vals <- predict(rf_model, newdata = rf_model$trainingData)
          obs_vals <- rf_model$trainingData$.outcome
          
          # Store predictions and observations for plotting
          predictions(data.frame(pred = pred_vals))
          observations(obs_vals)
          
          fitnocv = lm(pred_vals~obs_vals)
          fitres = summary(fitnocv)
        
          cat("R squared:", round(fitres$r.squared,3), "\n")
          cat("Adjusted R squared:",round(fitres$adj.r.squared,3), "\n")
          
        } else {
          preds <- rf_model$pred
          obs <- preds$obs
          
          # Store predictions and observations for plotting
          predictions(data.frame(pred = preds$pred))
          observations(obs)
          
          adj_r2 <- 1 - (1 - rf_model$results$Rsquared) * ((length(obs) - 1) / (length(obs) - ncol(rf_model$trainingData) - 1))
          
          cat("R squared:", round(rf_model$results$Rsquared, 3), "\n")
          cat("Adjusted R squared:", round(adj_r2, 3), "\n")
        }
      }
      
    }) # end output rf_metrics
    
    # output$rf_varimp <- renderTable({
    #   imp <- varImp(rf_model)$importance
    #   imp_df <- tibble::rownames_to_column(as.data.frame(imp), var = "Variable")
    #   dplyr::rename(imp_df, `Mean Impurity Decrease` = Overall)
    # })
    
    # Text message for 1 SD cutoff
    # output$rf_varimp_cutoff <- renderPrint({
    #   imp <- varImp(rf_model)$importance
    #   cutoff <- mean(imp$Overall) + sd(imp$Overall)
    #   cat("1 SD cutoff for Mean Impurity Decrease:", round(cutoff, 3), "\n")
    # })

    output$rf_plot <- renderPlot({
      if (input$rf_type == "Regression") {
        req(predictions(), observations())
        
        pred <- predictions()$pred
        obs <- observations()
       
        plot(obs, pred,
             xlab = "Actual",
             ylab = "Predicted",
             main = "Actual vs Predicted")
        
        abline(lm(pred ~ obs), col = "red", lwd = 2)
      } else {
        req(rocplot())
        plot(rocplot(), main = "Cross-Validated ROC", col="blue", lwd=3)
      }
    })
    
    # Download handler
    output$rf_save_model <- downloadHandler(
      filename = function() "rf_model.RData",
      content = function(file) {
        save(rf_model, file = file)
      })
    
  }) # end observe event

output$outcome_summary <- renderPrint({
  req(input$rf_datafile, input$rf_type, input$rf_outcome)
  
  df <- read.csv(input$rf_datafile$datapath)
  outcome <- input$rf_outcome
  
  val <- df[[outcome]]
  if ((is.numeric(val) && length(unique(val)) == 2) || (is.factor(val) && nlevels(val) == 2)) {
    cat("Class distribution of outcome variable:\n")
    print(table(df[[outcome]]))
  } else {
    cat("Mean of outcome variable:", round(mean(df[[outcome]], na.rm = TRUE), 3), "\n")
    cat("SD of outcome variable:", round(sd(df[[outcome]], na.rm = TRUE), 3), "\n")
  }
})

observeEvent(input$rf_predict_btn, {
  req(input$rf_model_file, input$rf_newdata)
  
  # Load model and new data
  load(input$rf_model_file$datapath)  # loads rf_model
  new_data <- read_csv(input$rf_newdata$datapath,show_col_types = FALSE)
  
  # Get outcome type from model object
  is_classification <- rf_model$modelType == "Classification"
  
  if (is_classification) {
    # For classification: predict class and probabilities
    pred_class <- predict(rf_model, newdata = new_data)
    pred_probs <- predict(rf_model, newdata = new_data, type = "prob")
    results <- cbind(new_data, Prediction = pred_class, pred_probs)
  } else {
    # For regression: predict numeric outcome only
    pred_vals <- predict(rf_model, newdata = new_data)
    results <- cbind(new_data, Prediction = pred_vals)
  }
  
  # Show in UI
  output$rf_predictions <- renderDT({
    datatable(results)
  })
  
  # Download handler
  output$rf_download_pred <- downloadHandler(
    filename = function() "rf_predictions.csv",
    content = function(file) {
      write_csv(results, file)
    }
  )
})

  output$rf_helpOutput <- renderUI({
    includeMarkdown(normalizePath("RF_Help.Rmd"))
  })

  output$rf_codeOutput <- renderText({
    req(input$rf_type, input$param_mode, input$cv_method)
    
    code <- c(
      "set.seed(42)  # for reproducibility",
      "",
      "library(randomForest)",
      "library(caret)",
      "library(pROC)",
      "",
      "df <- read.csv(\"path_to_your_file.csv\")",
      ""
    )
    
    if (input$rf_type == "Classification") {
      code <- c(code,
                "df$outcome <- as.factor(df$outcome)  # convert to factor",
                "")
    }
    
    if (input$rf_scale_data) {
      code <- c(code,
                "# Standardize numeric predictors (excluding outcome)",
                "numeric_vars <- sapply(df[, -which(names(df) == \"outcome\")], is.numeric)",
                "df[, numeric_vars] <- scale(df[, numeric_vars])",
                "")
    }
    
    if (input$class_balance == "Upsample") {
      code <- c(code,
                "# Controlled upsampling of the minority class",
                "minority_rows <- df[df$outcome == 'minority', ]",
                "upsampled <- minority_rows[sample(nrow(minority_rows), nrow(minority_rows), replace = TRUE), ]",
                "df <- rbind(df, upsampled, df[df$outcome != 'minority', ])",
                "")
    } else if (input$class_balance == "Downsample") {
      code <- c(code,
                "# Downsampling the majority class",
                "df <- downSample(x = df[, -which(names(df) == \"outcome\")],",
                "                 y = df$outcome, yname = \"outcome\")",
                "")
    }
    
    code <- c(code,
              "# Randomize the data",
              "df <- df[sample(nrow(df)), ]",
              "")
    
    code <- c(code,
              "# Define model formula",
              "form <- outcome ~ .",
              "")
    
    if (input$cv_method == "None") {
      cv <- "none"
    } else if (input$cv_method == "LOOCV") {
      cv <- "LOOCV"
    } else {
      cv <- paste0("cv (k = ", input$k_folds, ")")
    }
    
    code <- c(code,
              sprintf("# Set up cross-validation (%s)", cv),
              sprintf("ctrl <- trainControl(method = \"%s\", classProbs = TRUE, savePredictions = TRUE)",
                      ifelse(input$cv_method == "K-folds", "cv",
                             ifelse(input$cv_method == "LOOCV", "LOOCV", "none"))),
              "")
    
    if (input$param_mode == "Manual") {
      code <- c(code,
                "# Manual hyperparameters",
                sprintf("mtry <- %s", input$rf_mtry),
                sprintf("ntree <- %s", input$rf_ntree),
                sprintf("maxnodes <- %s", ifelse(is.null(input$rf_maxnodes), "NULL", input$rf_maxnodes)),
                "")
    } else {
      code <- c(code,
                "# Auto hyperparameters",
                "p <- number_of_predictors",
                if (input$rf_type == "Classification") {
                  "mtry <- round(sqrt(p))"
                } else {
                  "mtry <- round(p / 3)"
                },
                "ntree <- if (nrow(df) < 1000) 100 else 500",
                "maxnodes <- if (nrow(df) < 1000) NULL else 10",
                "")
    }
    
    code <- c(code,
              "# Train Random Forest",
              "",
              "model <- train(form, data = df, method = \"rf\",",
              "               trControl = ctrl,",
              "               tuneGrid = data.frame(mtry = mtry),",
              "               ntree = ntree, maxnodes = maxnodes)",
              "",
              "# Model Performance (Simplified)",
              if (input$rf_type == "Classification"){
                "confusionMatrix(model)"
              }
              else{
                "print(model)"
              }
              )
    
    paste(code, collapse = "\n")
  })
}

list(ui = rf_ui, server = rf_server)
}