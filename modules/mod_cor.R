mod_cor_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("method"),
      "Method",
      choices = c("Pearson" = "pearson", "Spearman" = "spearman")
    ),
    shiny::selectInput(
      ns("cols"),
      "Select variables to correlate",
      choices = NULL,
      multiple = TRUE
    ),
    actionButton(ns("go"), "Run correlation"),
    br(),
    textOutput(ns("ninfo")),
    br(),
    downloadButton(ns("dl"), "Download correlation matrix (CSV)"),
    br(),
    br(),
    verbatimTextOutput(ns("out")),
    plotOutput(ns("heatmap"), height = "500px"),
    br(),
    tags$small(
      strong("Rule of thumb: "),
      HTML(
        "|r| ≥ 0.7 = strong, 0.3–0.7 = moderate, < 0.3 = weak (Cohen, 1988)"
      ),
      br()
    )
  )
}

mod_cor_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      req(data())
      nums <- names(data())[sapply(data(), is.numeric)]
      updateSelectInput(session, "cols", choices = nums) # no default
    })

    # Compute on click (complete cases; Pearson default, Spearman optional)
    res <- eventReactive(input$go, {
      req(data(), length(input$cols) > 0)
      d <- data()[, input$cols, drop = FALSE]
      m <- input$method
      # n used under complete-case logic:
      n_used <- if (ncol(d) == 1) NA_integer_ else nrow(stats::na.omit(d))
      list(
        d = d,
        n = n_used,
        test = if (ncol(d) == 2) {
          stats::cor.test(d[[1]], d[[2]], method = m, use = "complete.obs")
        } else {
          NULL
        },
        mat = if (ncol(d) >= 2) {
          stats::cor(d, method = m, use = "complete.obs")
        } else {
          NULL
        }
      )
    })

    # Show sample size(s)
    output$ninfo <- renderText({
      req(res())
      if (!is.null(res()$test)) {
        # exactly two columns: n is just complete cases for the 2 vars
        n2 <- sum(stats::complete.cases(res()$d[, 1:2, drop = FALSE]))
        paste0("N used (complete cases for the selected pair): ", n2)
      } else if (!is.null(res()$mat)) {
        paste0("N used (complete cases across all selected columns): ", res()$n)
      } else {
        ""
      }
    })

    # Text output: cor.test() when 2 vars, otherwise matrix
    output$out <- renderPrint({
      req(res())
      if (!is.null(res()$test)) {
        res()$test
      } else if (!is.null(res()$mat)) {
        res()$mat
      } else {
        "Select at least two columns and click Run."
      }
    })

    # Heatmap (2×2 if two columns)
    output$heatmap <- renderPlot({
      req(res(), !is.null(res()$mat))

      df <- as.data.frame(as.table(res()$mat))
      names(df) <- c("Var1", "Var2", "value")

      ggplot2::ggplot(df, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.2f", value)),
          size = 3
        ) +
        ggplot2::scale_fill_gradient2(
          low = "#2166AC",
          mid = "#F7F7F7",
          high = "#B2182B",
          midpoint = 0,
          limits = c(-1, 1),
          name = "Correlation"
        ) +
        ggplot2::coord_fixed() +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank(),
          legend.position = "right"
        )
    })

    # Download the correlation matrix as CSV (not the chart)
    output$dl <- downloadHandler(
      filename = function() "correlation_matrix.csv",
      content = function(file) {
        req(res(), !is.null(res()$mat))
        utils::write.csv(res()$mat, file, row.names = TRUE)
      }
    )
  })
}
