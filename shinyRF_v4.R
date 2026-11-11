# Shiny RF app
make_rf_app <- function(){

# Class balancing is passed to caret via trainControl(sampling = ...) so it is
# applied only to the training portion of each fold, never to held-out data.
# Resamples the minority class with replacement, adding at most one extra copy
# of it (doubling) and never more than is needed to reach the size of the
# majority class. Setting the global environment keeps a saved model from
# capturing this app's workspace.
upsample_minority <- function(x, y) {
  counts <- table(y)
  minority_idx <- which(y == names(which.min(counts)))
  n_extra <- min(length(minority_idx), max(counts) - length(minority_idx))
  if (n_extra <= 0) return(list(x = x, y = y))
  extra <- minority_idx[sample.int(length(minority_idx), n_extra, replace = TRUE)]
  keep <- c(seq_along(y), extra)
  list(x = x[keep, , drop = FALSE], y = y[keep])
}
environment(upsample_minority) <- globalenv()

# Remove participant-level content from a trained caret model before it is saved
# for download. Prediction needs only the fitted forest and the factor levels
# stored in $xlevels. The formula's environment is reset because it otherwise
# carries the whole workspace in which the model was trained (including the data).
strip_participant_data <- function(model) {
  model$trainingData <- NULL
  model$pred <- NULL
  for (slot in c("y", "predicted", "votes", "oob.times", "inbag")) {
    model$finalModel[[slot]] <- NULL
  }
  attr(model$terms, ".Environment") <- globalenv()
  model
}

# ---- Performance metrics -------------------------------------------------
# Both helpers take observed values and predictions that were NOT produced by
# simply re-predicting the training rows (unless the caller labels them so).

# Classification. The ROC direction is fixed (higher probability = Class1) so
# that a model performing worse than chance is reported with AUC < 0.5 rather
# than being silently flipped by pROC's automatic direction detection.
class_metrics <- function(obs, pred, prob) {
  keep <- !is.na(pred) & !is.na(prob)
  obs <- obs[keep]; pred <- pred[keep]; prob <- prob[keep]
  cm <- confusionMatrix(pred, obs, positive = "Class1")
  roc_obj <- roc(response = obs, predictor = prob,
                 levels = c("Class0", "Class1"), direction = "<", quiet = TRUE)
  list(n_dropped = sum(!keep),
       class_dist = table(obs),
       table = cm$table,
       auc = as.numeric(auc(roc_obj)),
       f1 = unname(cm$byClass["F1"]),
       sens = unname(cm$byClass["Sensitivity"]),
       spec = unname(cm$byClass["Specificity"]),
       roc = roc_obj)
}

# Regression. R squared is the proportion of outcome variance explained by the
# predictions (1 - SSE/SST), which, unlike a squared correlation, is reduced by
# systematic bias in the predictions.
reg_metrics <- function(obs, pred) {
  keep <- !is.na(pred)
  obs <- obs[keep]; pred <- pred[keep]
  list(n_dropped = sum(!keep),
       r2 = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2),
       mae = mean(abs(obs - pred)),
       rmse = sqrt(mean((obs - pred)^2)),
       obs = obs, pred = pred)
}

print_class_metrics <- function(m, show_dist_f1 = TRUE) {
  if (show_dist_f1) {
    cat("Final class distribution for outcome variable:\n")
    print(m$class_dist)
    cat("\n")
  }
  cat("AUC:", round(m$auc, 3), "\n")
  if (show_dist_f1) cat("F1 Score:", round(m$f1, 3), "\n")
  cat("Sensitivity:", round(m$sens, 3), "\n")
  cat("Specificity:", round(m$spec, 3), "\n")
  cat("\nConfusion Matrix:\n")
  print(m$table)
}

print_reg_metrics <- function(m) {
  cat("R squared:", round(m$r2, 3), "\n")
  cat("MAE:", round(m$mae, 3), "\n")
  cat("RMSE:", round(m$rmse, 3), "\n")
}

# ---- Loading a saved model for prediction ---------------------------------
# An uploaded model file is untrusted input. It is loaded into an isolated
# environment (so nothing in it can replace functions used by the app), and only
# plain data are taken from it: the fitted forest's numeric arrays, the factor
# levels and the variable names. No function, formula or call stored in the file
# is ever evaluated; predictions are made with this app's own code.
is_plain_data <- function(x) {
  if (is.null(x)) return(TRUE)
  if (isS4(x)) return(FALSE)
  if (!all(vapply(attributes(x), is_plain_data, logical(1)))) return(FALSE)
  if (is.atomic(x)) return(TRUE)
  if (typeof(x) == "list") return(all(vapply(unclass(x), is_plain_data, logical(1))))
  FALSE
}

read_rf_model <- function(path) {
  not_model <- "The uploaded file is not a random forest model saved by Sandbox ML."
  e <- new.env(parent = emptyenv())
  loaded <- tryCatch(load(path, envir = e), error = function(err) character(0))
  if (!"rf_model" %in% loaded) stop(not_model)
  obj <- get("rf_model", envir = e, inherits = FALSE)
  if (!inherits(obj, "train") || typeof(obj) != "list") stop(not_model)
  obj <- unclass(obj)
  forest <- obj[["finalModel"]]
  if (!inherits(forest, "randomForest") || typeof(forest) != "list") stop(not_model)
  forest <- unclass(forest)
  forest$call <- NULL
  data_classes <- attr(obj[["terms"]], "dataClasses")
  model <- list(forest = forest,
                xlevels = obj[["xlevels"]],
                vars = names(data_classes)[-1],   # first entry is the outcome
                model_type = obj[["modelType"]])
  valid <- is_plain_data(model) &&
    is.character(model$vars) && length(model$vars) > 0 &&
    is.character(model$model_type) && length(model$model_type) == 1 &&
    model$model_type %in% c("Classification", "Regression") &&
    !is.null(rownames(forest$importance))
  if (!valid) stop(not_model)
  class(model$forest) <- "randomForest"
  model
}

# Returns the new data with predictions added. Rows that cannot be predicted
# (missing values, or a category the model never saw) get NA predictions.
predict_rf_model <- function(model, new_data) {
  new_data <- as.data.frame(new_data)
  # Models are trained on syntactic column names (see rf_data); accept either form
  lookup <- new_data
  if (!all(model$vars %in% names(lookup))) {
    names(lookup) <- make.names(names(lookup), unique = TRUE)
  }
  missing_vars <- setdiff(model$vars, names(lookup))
  if (length(missing_vars) > 0) {
    stop("New data are missing variables used by the model: ",
         paste(missing_vars, collapse = ", "))
  }
  nd <- lookup[model$vars]

  # CSV files don't preserve factor types, so realign categorical predictors
  # with the levels the model was trained on before predicting
  for (col in intersect(names(model$xlevels), model$vars)) {
    nd[[col]] <- factor(as.character(nd[[col]]), levels = model$xlevels[[col]])
  }

  ok <- complete.cases(nd)
  if (!any(ok)) {
    stop("No rows could be predicted: every row has missing values or categories not seen in training.")
  }
  X <- model.matrix(~ ., data = nd[ok, , drop = FALSE])
  x_names <- rownames(model$forest$importance)
  if (!all(x_names %in% colnames(X))) {
    stop("New data are not compatible with the model. Check that variable names and ",
         "types (numeric vs. categorical) match the data used to train the model.")
  }
  X <- X[, x_names, drop = FALSE]

  n <- nrow(nd)
  if (model$model_type == "Classification") {
    cls <- predict(model$forest, X)
    prob <- unclass(predict(model$forest, X, type = "prob"))
    Prediction <- factor(rep(NA_character_, n), levels = levels(cls))
    Prediction[ok] <- cls
    probs <- matrix(NA_real_, n, ncol(prob), dimnames = list(NULL, colnames(prob)))
    probs[ok, ] <- prob
    results <- cbind(new_data, Prediction = Prediction, as.data.frame(probs))
  } else {
    Prediction <- rep(NA_real_, n)
    Prediction[ok] <- predict(model$forest, X)
    results <- cbind(new_data, Prediction = Prediction)
  }
  list(results = results, n_skipped = sum(!ok))
}

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
                      "Maximum tree size - terminal nodes (maxnodes):",
                      choices = c("No limit" = "NULL", 5, 10, 15),
                      selected = "NULL")
        ),

        actionButton("run_rf", "Run Random Forest"),

        tags$hr(),
        tags$div(
          style = "text-align: center;",
          tags$img(src = "sb_icon.png", width = "80%", height = "auto"),
          tags$br(),
          tags$hr()
        ),
        helpText(tags$em("© Shelli Kesler 2025")),
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
            verbatimTextOutput("rf_predict_msg"),
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
rf_server <- function(input, output, session, app_state = NULL) {

  # Column names are made syntactic (as read.csv does) so they can be used in a
  # model formula and so the code shown in the Code tab behaves the same way.
  rf_data <- reactive({
    req(input$rf_datafile)
    df <- as.data.frame(read_csv(input$rf_datafile$datapath, show_col_types = FALSE,
                                 name_repair = "minimal"))
    names(df) <- make.names(names(df), unique = TRUE)
    df
  })

  output$rf_outcome_select <- renderUI({
    req(rf_data())
    selectInput("rf_outcome", "Select Outcome Variable:", choices = names(rf_data()))
  })

  output$rf_dataView <- DT::renderDataTable({
    rf_data()
  })

  # Everything shown in the Results and Plots tabs comes from this one object,
  # which is filled in only when "Run Random Forest" is clicked. The outputs
  # therefore always describe the model that was actually trained, even if the
  # sidebar options are changed afterwards.
  rf_result <- reactiveVal(NULL)

  rf_fail <- function(msg) {
    rf_result(list(error = msg))
    showNotification(msg, type = "error", duration = 10)
  }

  observe({
    req(input$cv_method)
    if (input$cv_method == "LOOCV") {
      updateRadioButtons(session, "class_balance", selected = "None")
      shinyjs::disable("class_balance")
    } else {
      shinyjs::enable("class_balance")
    }
  })

  observeEvent(input$run_rf, {
    req(rf_data(), input$rf_outcome)
    set.seed(42)

    df <- rf_data()
    outcome <- input$rf_outcome
    rf_type <- input$rf_type
    cv_method <- input$cv_method
    # Class balancing is not offered with LOOCV (the control is disabled)
    class_balance <- if (rf_type == "Classification" && cv_method != "LOOCV") input$class_balance else "None"

    # ---- Check the data and settings before training ----
    if (!outcome %in% names(df)) {
      return(rf_fail("The selected outcome variable was not found in the data."))
    }
    if (ncol(df) < 2) {
      return(rf_fail("The data must contain the outcome and at least one predictor."))
    }
    if (anyNA(df)) {
      return(rf_fail("The data contain missing values. Remove or impute them (see the Missing Data tab) before modeling."))
    }
    if (rf_type == "Classification") {
      vals <- as.character(unique(df[[outcome]]))
      if (!(length(vals) == 2 && all(vals %in% c("0", "1")))) {
        return(rf_fail("For classification, the outcome must be binary and coded 0/1 (1 = positive class)."))
      }
    } else if (!is.numeric(df[[outcome]])) {
      return(rf_fail("For regression, the outcome must be numeric."))
    }
    if (cv_method == "K-folds") {
      k_folds <- input$k_folds
      if (is.na(k_folds) || k_folds < 2 || k_folds > nrow(df)) {
        return(rf_fail("Number of folds must be at least 2 and no more than the number of samples."))
      }
    }
    if (input$param_mode == "Manual" &&
        (is.na(input$rf_mtry) || input$rf_mtry < 1 || is.na(input$rf_ntree) || input$rf_ntree < 1)) {
      return(rf_fail("mtry and number of trees must both be at least 1."))
    }

    # Convert all binary predictors to factors. The outcome is handled
    # separately so that a regression outcome is never turned into classes.
    is_binary <- function(x) length(unique(na.omit(x))) == 2
    binary_vars <- sapply(df, is_binary)
    binary_vars[outcome] <- FALSE
    for (v in names(df)[binary_vars]) df[[v]] <- as.factor(df[[v]])

    if (rf_type == "Classification") {
      # caret requires valid R variable names as factor levels for classProbs
      df[[outcome]] <- factor(as.character(df[[outcome]]),
                              levels = c("0", "1"), labels = c("Class0", "Class1"))
    }

    form <- as.formula(paste(outcome, "~ ."))

    if (input$param_mode == "Auto") {
      p <- ncol(df) - 1
      mtry <- if (rf_type == "Classification") max(1, round(sqrt(p))) else max(1, round(p / 3))
      ntree <- if (nrow(df) < 1000) 100 else 500
      maxnodes <- NULL  # no limit: grow trees fully
    } else {
      mtry <- input$rf_mtry
      ntree <- input$rf_ntree
      maxnodes <- if (input$rf_maxnodes == "NULL") NULL else as.numeric(input$rf_maxnodes)
    }

    use_class_probs <- rf_type == "Classification"
    sampling <- switch(class_balance, Upsample = upsample_minority, Downsample = "down", NULL)
    ctrl <- if (cv_method == "None") {
      trainControl(method = "none", classProbs = use_class_probs, savePredictions = TRUE, sampling = sampling)
    } else if (cv_method == "LOOCV") {
      trainControl(method = "LOOCV", classProbs = use_class_probs, savePredictions = TRUE, sampling = sampling)
    } else {
      trainControl(method = "cv", number = input$k_folds, classProbs = use_class_probs, savePredictions = TRUE, sampling = sampling)
    }

    # ---- Train the model and compute its performance ----
    res <- withProgress(message = "Training Random Forest model...", value = 0.3, {
      tryCatch({
        rf_model <- train(
          form,
          data = df,
          method = "rf",
          trControl = ctrl,
          tuneGrid = data.frame(mtry = mtry),
          ntree = ntree,
          maxnodes = maxnodes
        )
        incProgress(0.6)

        res <- list(model = rf_model, type = rf_type, cv = cv_method, balance = class_balance,
                    k_folds = if (cv_method == "K-folds") input$k_folds,
                    n = nrow(df), p = ncol(df) - 1, outcome = outcome,
                    mtry = mtry, ntree = ntree, maxnodes = maxnodes)

        if (cv_method == "None") {
          obs <- rf_model$trainingData$.outcome
          forest <- rf_model$finalModel

          # Training fit: the model predicting the rows it was trained on
          train_pred <- predict(rf_model, newdata = rf_model$trainingData)
          res$train <- if (use_class_probs) {
            class_metrics(obs, train_pred,
                          predict(rf_model, newdata = rf_model$trainingData, type = "prob")[["Class1"]])
          } else {
            reg_metrics(obs, train_pred)
          }

          # Out-of-bag (OOB) predictions: each row predicted only by trees that
          # did not use it. Not available with class balancing, because the
          # forest was then grown on resampled rows (and upsampled duplicates
          # would put copies of the same case both in and out of the bag).
          if (class_balance == "None") {
            res$oob <- if (use_class_probs) {
              class_metrics(obs, unname(forest$predicted), unname(forest$votes[, "Class1"]))
            } else {
              reg_metrics(obs, unname(forest$predicted))
            }
          }
        } else {
          held_out <- rf_model$pred
          res$cv_metrics <- if (use_class_probs) {
            class_metrics(held_out$obs, held_out$pred, held_out$Class1)
          } else {
            reg_metrics(held_out$obs, held_out$pred)
          }
        }

        # The Plots tab uses the most trustworthy predictions available
        if (!is.null(res$cv_metrics)) {
          res$plot_metrics <- res$cv_metrics
          res$plot_source <- "cross-validated (held-out) predictions"
        } else if (!is.null(res$oob)) {
          res$plot_metrics <- res$oob
          res$plot_source <- "out-of-bag (OOB) predictions"
        } else {
          res$plot_metrics <- res$train
          res$plot_source <- "training fit (optimistic)"
        }

        incProgress(1, detail = "Done!")
        res
      }, error = function(e) {
        list(error = paste("The model could not be trained:", conditionMessage(e)))
      })
    })

    if (!is.null(res$error)) return(rf_fail(res$error))
    rf_result(res)

    if (!is.null(app_state)) {
      app_state$last_output <- paste(
        sprintf("Random Forest (%s) — %d samples, %d predictors, outcome = %s",
                rf_type, res$n, res$p, outcome),
        sprintf("mtry = %s, ntree = %s, maxnodes = %s",
                mtry, ntree, ifelse(is.null(maxnodes), "no limit", maxnodes)),
        "(See the Results and Plots tabs for full metrics.)",
        sep = "\n"
      )
    }
  }) # end observe event

  output$rf_summary <- renderPrint({
    res <- rf_result()
    req(res)
    if (!is.null(res$error)) {
      cat("Error:", res$error, "\n")
      return(invisible())
    }
    cat("Number of samples:", res$n, "\n")
    cat("Number of predictors:", res$p, "\n")
    cat("Outcome variable:", res$outcome, "\n")
    cat("\nmtry:", res$mtry)
    cat("\nnum trees:", res$ntree)
    cat("\nmax terminal nodes:", ifelse(is.null(res$maxnodes), "no limit", res$maxnodes), "\n")
  })

  output$rf_metrics <- renderPrint({
    res <- rf_result()
    req(res, is.null(res$error))
    print_metrics <- function(m, ...) {
      if (res$type == "Classification") print_class_metrics(m, ...) else print_reg_metrics(m)
    }

    if (res$cv == "None") {
      cat("OUT-OF-BAG (OOB) ESTIMATES\n")
      if (!is.null(res$oob)) {
        cat("Each case is predicted using only the trees that did not see it during\n")
        cat("training, so these estimate how the model performs on new data.\n\n")
        print_metrics(res$oob)
        if (res$oob$n_dropped > 0) {
          cat("\nNote:", res$oob$n_dropped, "case(s) were in every tree's training sample and have no OOB\n")
          cat("prediction. Increase the number of trees.\n")
        }
      } else {
        cat("Not available when class balancing is used without cross-validation,\n")
        cat("because the forest is grown on resampled cases. Choose K-folds\n")
        cat("cross-validation to estimate performance on new data.\n")
      }
      cat("\n------------------------------------------------------------------\n")
      cat("TRAINING FIT\n")
      cat("The model predicting the same data it was trained on. This is optimistic\n")
      cat("and does NOT estimate performance on new data.\n\n")
      print_metrics(res$train)
    } else {
      if (res$cv == "LOOCV") {
        cat("CROSS-VALIDATED ESTIMATES (LOOCV)\n\n")
      } else {
        cat("CROSS-VALIDATED ESTIMATES (", res$k_folds, "-fold CV)\n\n", sep = "")
      }
      # Class distribution and F1 are suppressed for LOOCV
      print_metrics(res$cv_metrics, show_dist_f1 = res$cv != "LOOCV")
    }
    cat("\nPlots tab shows:", res$plot_source, "\n")
  }) # end output rf_metrics

  output$rf_plot <- renderPlot({
    res <- rf_result()
    req(res, is.null(res$error))
    m <- res$plot_metrics
    if (res$type == "Regression") {
      plot(m$obs, m$pred,
           xlab = "Actual",
           ylab = "Predicted",
           main = "Actual vs Predicted")
      abline(lm(m$pred ~ m$obs), col = "red", lwd = 2)
    } else {
      plot(m$roc, main = "ROC", col = "blue", lwd = 3)
    }
  })

  # Download handler
  output$rf_save_model <- downloadHandler(
    filename = function() "rf_model.RData",
    content = function(file) {
      res <- rf_result()
      req(res$model)
      rf_model <- strip_participant_data(res$model)
      save(rf_model, file = file)
    })

output$outcome_summary <- renderPrint({
  req(rf_data(), input$rf_outcome)
  val <- rf_data()[[input$rf_outcome]]
  req(!is.null(val))

  if (length(unique(na.omit(val))) == 2 || !is.numeric(val)) {
    cat("Class distribution of outcome variable:\n")
    print(table(val))
  } else {
    cat("Mean of outcome variable:", round(mean(val, na.rm = TRUE), 3), "\n")
    cat("SD of outcome variable:", round(sd(val, na.rm = TRUE), 3), "\n")
  }
})

pred_result <- reactiveVal(NULL)

observeEvent(input$rf_predict_btn, {
  req(input$rf_model_file, input$rf_newdata)

  out <- tryCatch({
    model <- read_rf_model(input$rf_model_file$datapath)
    new_data <- read_csv(input$rf_newdata$datapath, show_col_types = FALSE)
    predict_rf_model(model, new_data)
  }, error = function(e) list(error = conditionMessage(e)))

  if (!is.null(out$error)) showNotification(out$error, type = "error", duration = 10)
  pred_result(out)
})

output$rf_predict_msg <- renderPrint({
  out <- pred_result()
  req(out)
  if (!is.null(out$error)) {
    cat("Error:", out$error, "\n")
  } else if (out$n_skipped > 0) {
    cat(out$n_skipped, "row(s) could not be predicted (missing values, or a category not seen\n")
    cat("when the model was trained). Their predictions are blank.\n")
  } else {
    cat("Predictions made for all", nrow(out$results), "rows.\n")
  }
})

output$rf_predictions <- renderDT({
  out <- pred_result()
  req(out, is.null(out$error))
  datatable(out$results)
})

output$rf_download_pred <- downloadHandler(
  filename = function() "rf_predictions.csv",
  content = function(file) {
    out <- pred_result()
    req(out, is.null(out$error))
    write_csv(out$results, file)
  }
)

  output$rf_helpOutput <- renderUI({
    includeMarkdown(normalizePath("RF_Help.Rmd"))
  })

  output$rf_codeOutput <- renderText({
    req(input$rf_type, input$param_mode, input$cv_method, input$rf_outcome)

    is_class <- input$rf_type == "Classification"

    code <- c(
      "set.seed(42)  # for reproducibility",
      "",
      "library(randomForest)",
      "library(caret)",
      "library(pROC)",
      "",
      "df <- read.csv(\"path_to_your_file.csv\")",
      sprintf("outcome <- \"%s\"", input$rf_outcome),
      "",
      "# Convert binary predictors to factors",
      "is_binary <- function(x) length(unique(na.omit(x))) == 2",
      "binary_vars <- sapply(df, is_binary)",
      "binary_vars[outcome] <- FALSE  # the outcome is handled separately",
      "df[binary_vars] <- lapply(df[binary_vars], as.factor)",
      ""
    )

    if (is_class) {
      code <- c(code,
                "# Outcome must be coded 0/1. caret requires valid R variable names as",
                "# factor levels for classProbs, so 0/1 become Class0/Class1",
                "df[[outcome]] <- factor(df[[outcome]], levels = c(0, 1),",
                "                        labels = c(\"Class0\", \"Class1\"))",
                "")
    }

    code <- c(code,
              "# Define model formula",
              "form <- as.formula(paste(outcome, \"~ .\"))",
              "")

    balance <- if (is_class && input$cv_method != "LOOCV") input$class_balance else "None"

    if (balance == "Upsample") {
      code <- c(code,
                "# Controlled upsampling of the minority class: resamples it with replacement,",
                "# adding at most one extra copy (doubling) and never more than is needed to",
                "# match the majority class. caret applies this inside each training fold",
                "# only, so the held-out data used to evaluate the model are never duplicated.",
                "upsample_minority <- function(x, y) {",
                "  counts <- table(y)",
                "  minority_idx <- which(y == names(which.min(counts)))",
                "  n_extra <- min(length(minority_idx), max(counts) - length(minority_idx))",
                "  if (n_extra <= 0) return(list(x = x, y = y))",
                "  extra <- minority_idx[sample.int(length(minority_idx), n_extra, replace = TRUE)]",
                "  keep <- c(seq_along(y), extra)",
                "  list(x = x[keep, , drop = FALSE], y = y[keep])",
                "}",
                "")
    }
    sampling_arg <- switch(balance, Upsample = "upsample_minority", Downsample = "\"down\"", NULL)

    if (input$cv_method == "None") {
      cv <- "none"
    } else if (input$cv_method == "LOOCV") {
      cv <- "LOOCV"
    } else {
      cv <- paste0("cv (k = ", input$k_folds, ")")
    }

    class_probs <- if (is_class) "TRUE" else "FALSE"
    cv_method_arg <- ifelse(input$cv_method == "K-folds", "cv",
                             ifelse(input$cv_method == "LOOCV", "LOOCV", "none"))

    ctrl_args <- c(sprintf("method = \"%s\"", cv_method_arg),
                   if (input$cv_method == "K-folds") sprintf("number = %s", input$k_folds),
                   sprintf("classProbs = %s", class_probs),
                   "savePredictions = TRUE",
                   if (!is.null(sampling_arg)) sprintf("sampling = %s", sampling_arg))
    ctrl_line <- sprintf("ctrl <- trainControl(%s)", paste(ctrl_args, collapse = ", "))

    code <- c(code,
              sprintf("# Set up cross-validation (%s)", cv),
              if (!is.null(sampling_arg)) "# sampling = class balancing, applied to training data only",
              ctrl_line,
              "")

    if (input$param_mode == "Manual") {
      code <- c(code,
                "# Manual hyperparameters",
                sprintf("mtry <- %s", input$rf_mtry),
                sprintf("ntree <- %s", input$rf_ntree),
                sprintf("maxnodes <- %s  # maximum number of terminal nodes per tree",
                        ifelse(input$rf_maxnodes == "NULL", "NULL", input$rf_maxnodes)),
                "")
    } else {
      code <- c(code,
                "# Auto hyperparameters",
                "p <- ncol(df) - 1",
                if (is_class) {
                  "mtry <- max(1, round(sqrt(p)))"
                } else {
                  "mtry <- max(1, round(p / 3))"
                },
                "ntree <- if (nrow(df) < 1000) 100 else 500",
                "maxnodes <- NULL  # no limit on terminal nodes: grow trees fully",
                "")
    }

    code <- c(code,
              "# Train Random Forest",
              "",
              "model <- train(form, data = df, method = \"rf\",",
              "               trControl = ctrl,",
              "               tuneGrid = data.frame(mtry = mtry),",
              "               ntree = ntree, maxnodes = maxnodes)",
              "")

    # Performance code, mirroring what the Results tab reports
    class_perf <- function(pred, prob) {
      c(sprintf("confusionMatrix(%s, obs, positive = \"Class1\")", pred),
        "# levels and direction are fixed so an AUC below 0.5 is reported as such",
        sprintf("roc_obj <- roc(obs, %s, levels = c(\"Class0\", \"Class1\"), direction = \"<\")", prob),
        "auc(roc_obj)")
    }
    reg_perf <- function(pred) {
      c(sprintf("c(R2 = 1 - sum((obs - %s)^2) / sum((obs - mean(obs))^2),", pred),
        sprintf("  MAE = mean(abs(obs - %s)),", pred),
        sprintf("  RMSE = sqrt(mean((obs - %s)^2)))", pred))
    }

    if (input$cv_method != "None") {
      code <- c(code,
                "# Model Performance: cross-validated (held-out) predictions",
                "obs <- model$pred$obs",
                "preds <- model$pred$pred",
                if (is_class) {
                  c("probs <- model$pred$Class1  # predicted probability of the positive class",
                    class_perf("preds", "probs"),
                    "plot(roc_obj, main = \"ROC\")")
                } else {
                  c(reg_perf("preds"),
                    "plot(obs, preds, xlab = \"Actual\", ylab = \"Predicted\", main = \"Actual vs Predicted\")")
                })
    } else {
      code <- c(code, "obs <- model$trainingData$.outcome", "")
      if (balance == "None") {
        code <- c(code,
                  "# Model Performance: out-of-bag (OOB) predictions",
                  "# Each case is predicted using only the trees that did not see it during training",
                  "oob_preds <- model$finalModel$predicted",
                  if (is_class) {
                    c("oob_probs <- model$finalModel$votes[, \"Class1\"]",
                      class_perf("oob_preds", "oob_probs"),
                      "plot(roc_obj, main = \"ROC\")")
                  } else {
                    c(reg_perf("oob_preds"),
                      "plot(obs, oob_preds, xlab = \"Actual\", ylab = \"Predicted\", main = \"Actual vs Predicted\")")
                  },
                  "")
      } else {
        code <- c(code,
                  "# OOB estimates are not valid here because class balancing resampled the",
                  "# training cases. Use cross-validation to estimate performance on new data.",
                  "")
      }
      code <- c(code,
                "# Training fit: the model predicting its own training data.",
                "# This is optimistic and does not estimate performance on new data.",
                "train_preds <- predict(model, newdata = model$trainingData)",
                if (is_class) {
                  c("train_probs <- predict(model, newdata = model$trainingData, type = \"prob\")[[\"Class1\"]]",
                    class_perf("train_preds", "train_probs"))
                } else {
                  reg_perf("train_preds")
                })
    }

    paste(code, collapse = "\n")
  })
}

list(ui = rf_ui, server = rf_server)
}
