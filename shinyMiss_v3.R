# Shiny Missing Data App

make_miss_app <- function() {
miss_ui <- tagList(
  titlePanel("Missing Data Analysis"),
 
  sidebarLayout(
    sidebarPanel(
      helpText(tags$em("Data file should be cleaned.")),
      helpText(tags$em("If the data will be used for predictive modeling, remove the outcome variable (and any cases missing the outcome) before imputing.")),
      fileInput("miss_datafile", "Upload CSV File", accept = ".csv"),
      
      tags$hr(),
      tags$h4("Visualize Data"),
    
      actionButton("plot_miss", "Plot Data"),
      
      tags$hr(),
      tags$h4("Imputation"),
      
      selectInput("miss_percent",
                  "Missing % Threshold",
                  choices = c("None" = 0, 5, 10, 20),
                  selected = 10),
      
      radioButtons("miss_type", "Imputation Strategy",
                   choices = c("Listwise Deletion", "Single Stat", "Random Forest Imputation")),
      
      actionButton("impute_miss", "Impute Missing Data"),
      
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
        tabPanel("Missing Data",
                 tags$p("Tip: right click on a plot to save it.", 
                        style = "color: gray; font-style: italic; margin-top: 10px;"),
                 plotOutput("miss_plot"),
                 verbatimTextOutput("little_mcar"),
                 plotOutput("upset_plot")
        ),
        
        tabPanel("Imputed Data",
                 tags$p("Tip: right click a plot to save it.", 
                        style = "color: gray; font-style: italic; margin-top: 10px;"),
                 plotOutput("imputed_observed"),
                 verbatimTextOutput("excluded_vars"),
                 downloadButton("miss_download_imputed", "Download Imputed Data")
                 ),
        
        tabPanel("Data",
                 h4("Uploaded Dataset"),
                 DT::dataTableOutput("miss_dataView")
        ),
        
        tabPanel("Code",
                 h4("Missing Data Analysis Code"),
                 verbatimTextOutput("miss_codeOutput")
        ),
        
        tabPanel("Help",
                 h4(""),
                 uiOutput("miss_helpOutput")
        )

      )
    )
  )   
)

# Server
miss_server <- function(input, output, session) {
  miss_data <- reactive({
    req(input$miss_datafile)
    read_csv(input$miss_datafile$datapath,show_col_types = FALSE)
  })
  
  # Result of the most recent imputation. 
  imp_result <- reactiveVal(NULL)

  output$miss_dataView <- DT::renderDataTable({
    req(input$miss_datafile)
    df <- read.csv(input$miss_datafile$datapath)
    df
  })
  
  observeEvent(input$plot_miss, {
    
    df <- miss_data()
    
   output$miss_plot <- renderPlot({
      req(df)
     
     missing_df <- df %>%
       summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
       pivot_longer(cols = everything(), names_to = "Variable", values_to = "PercentMissing")
     
     # Sort and plot
     ggplot(missing_df, aes(x = reorder(Variable, PercentMissing), y = PercentMissing)) +
       geom_point(color = "#00688B", size = 2) +
       geom_segment(aes(xend = Variable, y = 0, yend = PercentMissing), color = "#00688B") +
       coord_flip() +
       labs(x = "Variables", y = "% Missing", title = "Percent Missing by Variable") +
       theme_minimal(base_size = 14)
      
    })
    
   output$upset_plot <- renderPlot({
     req(df)
     # Check for missing data
     has_missing <- anyNA(df)
     validate(
       need(has_missing, "Upset plot not valid. No missing data found."),
       need(sum(colSums(is.na(df)) > 0) >= 2,
            "Upset plot requires at least two variables with missing data.")
     )
     # If missing values exist, show the plot
     gg_miss_upset(df)
   })
    
    
   output$little_mcar <- renderPrint({
     req(df)
     
     # Check if all columns are numeric or integer
     is_numeric_df <- all(sapply(df, function(col) is.numeric(col) || is.integer(col)))
     has_missing <- anyNA(df)
     
     if (!has_missing) {
       cat("Little's MCAR test not applicable: no missing data found.\n")
     } else if (!is_numeric_df) {
       cat("Little's MCAR test not valid: dataset contains non-numeric columns.\n")
     } else {
       littletest <- mcar_test(df)
       cat("Little's MCAR Test:\n")
       cat("Chi Sq:", littletest$statistic, "\n")
       cat("p value:", littletest$p.value, "\n")
     }
   })
    
  })
 
  
  # Whole-number variables with few distinct values are treated as ordinal. 
  is_ordinal_like <- function(x) {
    obs <- x[!is.na(x)]
    is.numeric(x) && length(obs) > 0 && all(obs == round(obs)) && length(unique(obs)) <= 10
  }

  impute_single_stat <- function(df) {
    df_imputed <- df  # preserve original
    
    for (col in names(df)) {
      x <- df[[col]]
      miss <- is.na(x)
      if (!any(miss) || all(miss)) next
      
      if (is.numeric(x)) {
        # Ordinal: median (type = 1 always returns an observed category)
        # Other numeric: mean
        fill <- if (is_ordinal_like(x)) {
          as.numeric(quantile(x, 0.5, type = 1, na.rm = TRUE))
        } else {
          mean(x, na.rm = TRUE)
        }
        df_imputed[[col]][miss] <- fill
        
        # Categorical (binary or nominal factor/character/logical): mode
      } else if (is.factor(x) || is.character(x) || is.logical(x)) {
        mode_val <- names(which.max(table(x)))
        if (is.logical(x)) mode_val <- as.logical(mode_val)
        df_imputed[[col]][miss] <- mode_val
        if (is.factor(df_imputed[[col]])) df_imputed[[col]] <- droplevels(df_imputed[[col]])
      }
    }
    
    return(df_imputed)
  }
  
  plot_imputed_observed <- function(df, df_imputed, miss_vars) {
    plots <- list()
    
    for (var in miss_vars) {
      if (!var %in% names(df_imputed)) next
      if (all(is.na(df[[var]])) || all(is.na(df_imputed[[var]]))) next
      
      # Get indices of missing values in original df
      miss_idx <- which(is.na(df[[var]]))
      
      # Construct value vector
      value <- df[[var]]
      value[miss_idx] <- df_imputed[[var]][miss_idx]  # replace missing rows only
      
      # Create combined plot data
      df_plot <- data.frame(
        imputed = ifelse(is.na(df[[var]]), "Imputed", "Observed"),
        value = value
      )
      
      # Create and store the plot
      p <- ggplot(df_plot, aes(x = imputed, y = value, color = imputed)) +
        geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7, size = 1.5) +
        labs(title = var, x = NULL, y = NULL) +
        scale_color_manual(values = c("Imputed" = "#F8766D", "Observed" = "#00BFC4")) +
        theme_minimal()
      
      plots[[var]] <- p
    }
    
    return(plots)
  }
  
  observeEvent(input$impute_miss, {
    
    df <- miss_data()
    method <- input$miss_type
    threshold <- as.numeric(input$miss_percent)
    n_original <- nrow(df)
    
    imputed_result <- withProgress(message = "Imputing data", value = 0, {
      
      incProgress(0.5)
      
      # Convert all binary variables to factors
      is_binary <- function(x) length(unique(na.omit(x))) == 2
      binary_vars <- sapply(df, is_binary)
      df[, binary_vars] <- lapply(df[, binary_vars, drop = FALSE], as.factor)
      
      # Exclude high-missing variables if specified
      percent_missing <- colSums(is.na(df)) / nrow(df) * 100
      excluded <- if (threshold != 0) names(df)[percent_missing > threshold] else character(0)
      df <- df[, setdiff(names(df), excluded), drop = FALSE]
      
      # pmm.k = 5 (predictive mean matching): each imputed value is drawn from
      # observed values with similar predictions, so imputed values are realistic
      # (e.g., whole numbers stay whole) and keep the spread of the observed data.
      result <- tryCatch(
        switch(method,
               "Listwise Deletion" = df[complete.cases(df), ],
               "Single Stat" = impute_single_stat(df),
               "Random Forest Imputation" = missRanger(df, num.trees = 100, pmm.k = 5, verbose = 0, seed = 42)
        ),
        error = function(e) {
          showNotification(paste("Imputation failed:", conditionMessage(e)), type = "error", duration = 10)
          NULL
        })
      
      incProgress(1, detail = "Done!")
      result
    })
    
    req(imputed_result)
    
    imp_result(list(
      method = method,
      threshold = threshold,
      excluded = excluded,
      n_original = n_original,
      df_clean = df,                # after exclusions, before imputation
      df_imputed = imputed_result,
      # variables that still contain missing values (entirely missing, or a type
      # the method does not handle, such as dates)
      not_imputed = if (method == "Listwise Deletion") character(0) else
        names(imputed_result)[sapply(imputed_result, anyNA)]
    ))
   
  }) # end observe impute 
  
  output$excluded_vars <- renderPrint({
    res <- imp_result()
    req(res)
    
    if (res$threshold == 0) {
      cat("No variables were excluded (no missingness threshold selected).")
    } else if (length(res$excluded) > 0) {
      cat("Excluded variables due to high missingness (> ", res$threshold, "%):\n", 
          paste(res$excluded, collapse = ", "))
    } else {
      cat("No variables exceeded the missingness threshold of", res$threshold, "%.")
    }
    
    if (res$method == "Listwise Deletion") {
      cat("\n\nListwise deletion removed", res$n_original - nrow(res$df_imputed), "of",
          res$n_original, "cases;", nrow(res$df_imputed), "complete cases remain.")
    }
    if (length(res$not_imputed) > 0) {
      cat("\n\nNot imputed (entirely missing, or a variable type this method does not handle):\n",
          paste(res$not_imputed, collapse = ", "))
    }
  })
  
  output$imputed_observed <- renderPlot({
    res <- imp_result()
    req(res)
    
    # Listwise deletion removes cases; no values are imputed, so there is
    # nothing to compare with the observed values.
    if (res$method == "Listwise Deletion") {
      plot.new()
      text(0.5, 0.5, "Listwise deletion removes cases with missing data.\nNo values were imputed, so there is nothing to plot.", cex = 1.2)
      return(invisible())
    }
    
    miss_vars <- names(res$df_clean)[sapply(res$df_clean, function(x) any(is.na(x)))]
    plots <- plot_imputed_observed(res$df_clean, res$df_imputed, miss_vars)
    
    if (length(plots) == 0) {
      plot.new()
      text(0.5, 0.5, "No variables had missing values to plot.", col = "red", cex = 1.2)
    } else {
      grid.arrange(grobs = plots, ncol = 3)
    }
    
  })
  
  # Download handler
  output$miss_download_imputed <- downloadHandler(
    filename = function() "imputed_data.csv",
    content = function(file) {
      req(imp_result())
      write_csv(imp_result()$df_imputed, file)
    }
  )
    
    output$miss_codeOutput <- renderText({
      
      req(input$miss_percent, input$miss_type)
      
      code <- c()
      code <- c(code, "df <- read.csv(\"path_to_your_file.csv\")",
                "",
                "library(naniar)",
                "library(ggplot2)",
                "library(dplyr)",
                "library(tidyr)",
                if (input$miss_type == "Random Forest Imputation") "library(missRanger)",
                "",
                "# Calculate percent missing per variable",
                "missing_df <- df %>%",
                "  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%",
                "  pivot_longer(cols = everything(), names_to = \"Variable\", values_to = \"PercentMissing\")",
                "",
                "# Plot percent missingness",
                "ggplot(missing_df, aes(x = reorder(Variable, PercentMissing), y = PercentMissing)) +",
                "  geom_point(color = \"#00688B\", size = 2) +",
                "  geom_segment(aes(xend = Variable, y = 0, yend = PercentMissing), color = \"#00688B\") +",
                "  coord_flip() +",
                "  labs(x = \"Variables\", y = \"% Missing\", title = \"Percent Missing by Variable\") +",
                "  theme_minimal(base_size = 14)",
                "",
                "# Plot relationships in missingness",
                "gg_miss_upset(df)",
                "",
                "# Convert binary variables to factors",
                "is_binary <- function(x) length(unique(na.omit(x))) == 2",
                "binary_vars <- sapply(df, is_binary)",
                "df[binary_vars] <- lapply(df[binary_vars], as.factor)",
                ""
      )
      
      if (as.numeric(input$miss_percent) != 0) {
        code <- c(code,
                  paste0("# Exclude variables with >", input$miss_percent, "% missingness",
                         "\npercent_missing <- colSums(is.na(df)) / nrow(df) * 100",
                         "\ndf <- df[, percent_missing <= ", input$miss_percent, ", drop = FALSE]"),
                  ""
        )
      }
      
      if (input$miss_type == "Listwise Deletion") {
        code <- c(code, "# Listwise deletion",
                  "df_imputed <- df[complete.cases(df), ]",
                  "")
      } else if (input$miss_type == "Single Stat") {
        code <- c(code,
                  "# Whole-number variables with 10 or fewer distinct values (e.g., Likert items)",
                  "# are treated as ordinal",
                  "is_ordinal_like <- function(x) {",
                  "  obs <- x[!is.na(x)]",
                  "  is.numeric(x) && length(obs) > 0 && all(obs == round(obs)) && length(unique(obs)) <= 10",
                  "}",
                  "",
                  "# Define single-stat imputation function",
                  "impute_single_stat <- function(df) {",
                  "  df_imputed <- df  # preserve original",
                  "  for (col in names(df)) {",
                  "    x <- df[[col]]",
                  "    miss <- is.na(x)",
                  "    if (!any(miss) || all(miss)) next",
                  "    if (is.numeric(x)) {",
                  "      # ordinal: median (type = 1 returns an observed category); other numeric: mean",
                  "      fill <- if (is_ordinal_like(x)) {",
                  "        as.numeric(quantile(x, 0.5, type = 1, na.rm = TRUE))",
                  "      } else {",
                  "        mean(x, na.rm = TRUE)",
                  "      }",
                  "      df_imputed[[col]][miss] <- fill",
                  "    } else if (is.factor(x) || is.character(x) || is.logical(x)) {",
                  "      # categorical: mode",
                  "      mode_val <- names(which.max(table(x)))",
                  "      if (is.logical(x)) mode_val <- as.logical(mode_val)",
                  "      df_imputed[[col]][miss] <- mode_val",
                  "      if (is.factor(df_imputed[[col]])) df_imputed[[col]] <- droplevels(df_imputed[[col]])",
                  "    }",
                  "  }",
                  "",
                  "  return(df_imputed)",
                  "}",
                  "",
                  "# Run single stat imputation",
                  "df_imputed <- impute_single_stat(df)",
                  ""
        )
      } else {
        code <- c(code,
                  "# Random forest imputation with predictive mean matching (pmm.k = 5), which",
                  "# draws each imputed value from observed values with similar predictions",
                  "df_imputed <- missRanger(df, num.trees = 100, pmm.k = 5, verbose = 0, seed = 42)",
                  ""
        )
      }
      
      if (input$miss_type == "Listwise Deletion") {
        code <- c(code,
                  "# Number of cases removed (no values are imputed with listwise deletion)",
                  "nrow(df) - nrow(df_imputed)",
                  "")
      } else {
      code <- c(code,
                "# Visualize observed vs. imputed values for one variable",
                "var <- \"var_to_plot\"  # replace with variable of interest",
                "",
                "df_plot <- df %>%",
                "  mutate(",
                "    imputed = ifelse(is.na(.data[[var]]), \"Imputed\", \"Observed\"),",
                "    value = ifelse(is.na(.data[[var]]), df_imputed[[var]], .data[[var]])",
                "  )",
                "",
                "ggplot(df_plot, aes(x = imputed, y = value, color = imputed)) +",
                "  geom_jitter(width = 0.2, alpha = 0.7, size = 1.5) +",
                "  labs(title = var, x = NULL, y = NULL) +",
                "  scale_color_manual(values = c(\"Imputed\" = \"#F8766D\", \"Observed\" = \"#00BFC4\")) +",
                "  theme_minimal()",
                ""
      )
      }
      
      code <- c(code,
                "# Save imputed file",
                "write.csv(df_imputed, file = \"path_to_file.csv\", row.names = FALSE)"
      )
      
      paste(code, collapse = "\n")
    }) # end code output
  
 
  output$miss_helpOutput <- renderUI({
    includeMarkdown(normalizePath("Miss_Data_Help.Rmd"))
  })
  
}

list(ui = miss_ui, server = miss_server)
}