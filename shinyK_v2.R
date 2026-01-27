# Shiny Clustering App

make_clust_app <- function() {

  clust_ui <- shiny::tagList(
    shiny::titlePanel("Clustering Analysis"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::helpText(shiny::tags$em(
          "File should contain only the variables selected for clustering and have no missing data."
        )),
        shiny::fileInput("clust_datafile", "Upload CSV File", accept = ".csv"),

        shiny::selectInput(
          "method", "Clustering Method:",
          choices = c(
            "K-means (Euclidean)",
            "K-medoids (Manhattan)",
            "K-medoids (Gower)"
          )
        ),

        shiny::radioButtons(
          "k_mode", "Choose number of clusters:",
          choices = c("Auto", "Manual"),
          selected = "Auto"
        ),

        shiny::conditionalPanel(
          condition = "input.k_mode == 'Manual'",
          shiny::numericInput("clusters", "Number of Clusters (k):", value = 2, min = 2)
        ),

        shiny::checkboxInput("scale_data", "Standardize the data?", FALSE),

        shiny::actionButton("run", "Run Clustering"),

        shiny::tags$hr(),
        shiny::tags$div(
          style = "text-align: center;",
          shiny::tags$img(src = "sb_icon.png", width = "80%", height = "auto"),
          shiny::tags$br(),
          shiny::tags$hr()
        ),
        shiny::helpText(shiny::tags$em("© Shelli Kesler 2025"))
      ),

      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Plots",
            shiny::tags$p(
              "Tip: right click on a plot to save it.",
              style = "color: gray; font-style: italic; margin-top: 10px;"
            ),
            shiny::plotOutput("silPlot"),
            shiny::verbatimTextOutput("silMean"),
            shiny::plotOutput("pcaPlot", height = "600px"),
            shiny::downloadButton("k_download_clust", "Download Clusters"),
            shiny::downloadButton("k_download_model", "Download Cluster Model")
          ),

          shiny::tabPanel(
            "Code",
            shiny::h4("Clustering Code"),
            shiny::tags$p(
              "Tip: you must run clustering before code will be generated.",
              style = "color: gray; font-style: italic; margin-top: 10px;"
            ),
            shiny::verbatimTextOutput("clust_codeOutput")
          ),

          shiny::tabPanel(
            "Data",
            shiny::h4("Uploaded Dataset"),
            DT::dataTableOutput("clust_dataView")
          ),

          shiny::tabPanel(
            "Help",
            shiny::h4(""),
            shiny::uiOutput("clust_helpOutput")
          )
        )
      )
    )
  )

  # Server
  clust_server <- function(input, output, session) {

    clust_data <- shiny::reactive({
      shiny::req(input$clust_datafile)
      df <- utils::read.csv(input$clust_datafile$datapath)
      # Keep numeric columns only (as in original)
      df[sapply(df, is.numeric)]
    })

    output$clust_dataView <- DT::renderDataTable({
      shiny::req(input$clust_datafile)
      utils::read.csv(input$clust_datafile$datapath)
    })

    clustering_result <- shiny::eventReactive(input$run, {
      set.seed(42)

      shiny::withProgress(message = "Clustering the data...", value = 0, {

        df_raw <- clust_data()
        shiny::validate(shiny::need(nrow(df_raw) > 1, "Uploaded data must have at least 2 rows."))
        shiny::validate(shiny::need(ncol(df_raw) > 0, "Uploaded data must have at least 1 numeric column."))

        # Detect variable types
        is_binary <- function(x) is.numeric(x) && all(x %in% c(0, 1)) && length(unique(x)) == 2
        binary_vars <- sapply(df_raw, is_binary)
        continuous_vars <- !binary_vars

        # Conditional scaling (scale continuous variables only)
        df <- if (isTRUE(input$scale_data)) {
          df_scaled <- df_raw
          if (any(continuous_vars)) {
            df_scaled[, continuous_vars] <- scale(df_raw[, continuous_vars])
          }
          as.data.frame(df_scaled)
        } else {
          df_raw
        }

        shiny::incProgress(0.2)

        method <- input$method
        k_mode <- input$k_mode

        # Compute distance matrix only for PAM-based methods
        dist_mat <- if (method == "K-means (Euclidean)") {
          NULL
        } else if (method == "K-medoids (Manhattan)") {
          stats::dist(df, method = "manhattan")
        } else {
          cluster::daisy(df, metric = "gower")
        }

        shiny::incProgress(0.3)

        # Determine optimal k if selected
        if (k_mode == "Auto") {
          sil_width <- rep(NA_real_, 10)

          if (method == "K-means (Euclidean)") {
            for (i in 2:10) {
              fit <- stats::kmeans(df, centers = i, nstart = 25)
              sil <- cluster::silhouette(fit$cluster, stats::dist(df))
              sil_width[i] <- mean(sil[, 3])
            }
            k <- which.max(sil_width)
          } else {
            for (i in 2:10) {
              fit <- cluster::pam(dist_mat, k = i, diss = TRUE)
              sil_width[i] <- fit$silinfo$avg.width
            }
            k <- which.max(sil_width)
          }
        } else {
          k <- input$clusters
        }

        shiny::validate(shiny::need(is.finite(k) && k >= 2, "k must be at least 2."))
        shiny::validate(shiny::need(k < nrow(df), "k must be smaller than the number of rows."))

        shiny::incProgress(0.5)

        # Run clustering + silhouette
        if (method == "K-means (Euclidean)") {
          clust_model <- stats::kmeans(df, centers = k, nstart = 25)
          clusters <- clust_model$cluster
          sil <- cluster::silhouette(clusters, stats::dist(df))
        } else {
          clust_model <- cluster::pam(dist_mat, k = k, diss = TRUE, medoids = "random", nstart = 25)
          clusters <- clust_model$clustering
          sil <- cluster::silhouette(clusters, dist_mat)
        }

        mean_sil <- mean(sil[, 3])

        # Visualization
        clust_obj <- list(cluster = clusters)
        class(clust_obj) <- "kmeans"  # works with factoextra::fviz_cluster
        pca_plot <- factoextra::fviz_cluster(
          clust_obj,
          data = df,
          geom = "point",
          ellipse.type = "norm",
          ggtheme = ggplot2::theme_minimal(),
          main = "PCA Plot"
        )

        # Output dataset (raw, unstandardized + cluster assignment)
        df_out <- df_raw
        df_out$cluster <- clusters

        shiny::incProgress(1, detail = "Done!")

        list(
          sil = sil,
          mean_sil = mean_sil,
          pca_plot = pca_plot,
          model = clust_model,
          df_out = df_out,
          k = k,
          method = method,
          scaled = isTRUE(input$scale_data)
        )
      })
    })

    output$silPlot <- shiny::renderPlot({
      shiny::req(clustering_result())
      factoextra::fviz_silhouette(clustering_result()$sil) +
        ggplot2::ggtitle("Silhouette Plot")
    })

    output$silMean <- shiny::renderPrint({
      shiny::req(clustering_result())
      cat("Mean Silhouette Width:", round(clustering_result()$mean_sil, 3))
    })

    output$pcaPlot <- shiny::renderPlot({
      shiny::req(clustering_result())
      print(clustering_result()$pca_plot)
    })

    # Download clustered data
    output$k_download_clust <- shiny::downloadHandler(
      filename = function() "data_clusters.csv",
      content = function(file) {
        shiny::req(clustering_result())
        readr::write_csv(clustering_result()$df_out, file)
      }
    )

    # Download cluster model object
    output$k_download_model <- shiny::downloadHandler(
      filename = function() "cluster_model.Rdata",
      content = function(file) {
        shiny::req(clustering_result())
        clust_model <- clustering_result()$model
        save(clust_model, file = file)
      }
    )

    output$clust_codeOutput <- shiny::renderText({
      shiny::req(input$run > 0)
      method <- input$method
      k_mode <- input$k_mode
      scale_on <- input$scale_data
      num_clust <- input$clusters

      code_lines <- c(
        "set.seed(42)  # ensures reproducibility",
        "",
        "library(cluster)",
        "library(factoextra)",
        "library(ggplot2)",
        "library(dplyr)",
        "library(tidyr)",
        "",
        "df_raw <- read.csv(\"path_to_your_file.csv\")  # load data",
        "",
        "# detect variable type",
        "is_binary <- function(x) is.numeric(x) && all(x %in% c(0, 1)) && length(unique(x)) == 2",
        "binary_vars <- sapply(df_raw, is_binary)",
        "continuous_vars <- !binary_vars",
        ""
      )

      if (isTRUE(scale_on)) {
        code_lines <- c(
          code_lines,
          "",
          "# scale continuous variables only",
          "df_scaled <- df_raw",
          "df_scaled[, continuous_vars] <- scale(df_raw[, continuous_vars])",
          "df <- as.data.frame(df_scaled)",
          ""
        )
      } else {
        code_lines <- c(code_lines, "df <- df_raw", "")
      }

      if (k_mode == "Auto") {
        if (method == "K-means (Euclidean)") {
          code_lines <- c(
            code_lines,
            "# determine optimal number of clusters using k-means",
            "sil_width <- rep(NA, 10)",
            "for (i in 2:10) {",
            "  fit <- kmeans(df, centers = i, nstart = 25)",
            "  sil <- silhouette(fit$cluster, dist(df))",
            "  sil_width[i] <- mean(sil[, 3])",
            "}",
            "k <- which.max(sil_width)"
          )
        } else {
          code_lines <- c(
            code_lines,
            "# determine optimal number of clusters using PAM",
            "sil_width <- rep(NA, 10)",
            "# define dist_mat based on your chosen metric, then:",
            "for (i in 2:10) {",
            "  fit <- pam(dist_mat, k = i, diss = TRUE)",
            "  sil_width[i] <- fit$silinfo$avg.width",
            "}",
            "k <- which.max(sil_width)"
          )
        }
      } else {
        code_lines <- c(code_lines, paste0("k <- ", num_clust))
      }

      if (method == "K-means (Euclidean)") {
        code_lines <- c(
          code_lines,
          "",
          "# k-means clustering",
          "clust_model <- kmeans(df, centers = k, nstart = 25)"
        )
      } else if (method == "K-medoids (Manhattan)") {
        code_lines <- c(
          code_lines,
          "",
          "# manhattan distance for PAM",
          "dist_mat <- dist(df, method = 'manhattan')",
          "clust_model <- pam(dist_mat, k = k, diss = TRUE, medoids = 'random', nstart = 25)"
        )
      } else {
        code_lines <- c(
          code_lines,
          "",
          "# gower distance for PAM",
          "dist_mat <- daisy(df, metric = 'gower')",
          "clust_model <- pam(dist_mat, k = k, diss = TRUE, medoids = 'random', nstart = 25)"
        )
      }

      code_lines <- c(
        code_lines,
        "",
        "# Plot Cluster Result",
        "sil <- silhouette(clust_model$cluster, dist(df))",
        "fviz_silhouette(sil)",
        "",
        "# Save data with clusters",
        "df_out <- df_raw",
        "df_out$cluster <- clust_model$cluster",
        "write.csv(df_out, file = 'clustered_data.csv', row.names = FALSE)"
      )

      paste(code_lines, collapse = "\n")
    })

    output$clust_helpOutput <- shiny::renderUI({
      help_path <- "Clustering_Help.Rmd"
      if (file.exists(help_path)) {
        shiny::includeMarkdown(help_path)
      } else {
        shiny::HTML(
          "Help file <code>Clustering_Help.Rmd</code> was not found in the deployed app directory."
        )
      }
    })
  }

  list(ui = clust_ui, server = clust_server)
}
