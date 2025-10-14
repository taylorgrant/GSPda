# Basic EDA Module (v0.1)
# ------------------------------------------------------------
# UI:     mod_eda_ui(id)
# Server: mod_eda_server(id, data)
#
# `data` should be a reactive that returns a data.frame/tibble.
# Features (keep it lean):
#  - Variable picker to show distribution (auto-chooses plot type by variable class)
#  - Summary table for the selected variable (numeric: mean/sd/quantiles; categorical: counts/props)
#  - Scatterplot for any two variables (numeric-only), optional smoother
# ------------------------------------------------------------

mod_eda_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tabsetPanel(
      id = ns("eda_tabs"),
      type = "tabs",
      shiny::tabPanel(
        "Distributions",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::uiOutput(ns("var_picker")),
            shiny::uiOutput(ns("dist_controls"))
          ),
          shiny::column(
            8,
            shiny::plotOutput(ns("dist_plot"), height = 320),
            shiny::h5("Summary"),
            shiny::tableOutput(ns("var_summary"))
          )
        )
      ),
      shiny::tabPanel(
        "Scatter",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::uiOutput(ns("x_picker")),
            shiny::uiOutput(ns("y_picker")),
            shiny::checkboxInput(
              ns("add_smoother"),
              "Add smoother (loess)",
              FALSE
            ),
            shiny::sliderInput(
              ns("alpha"),
              "Point alpha",
              min = 0.1,
              max = 1,
              value = 0.6,
              step = 0.1
            )
          ),
          shiny::column(8, shiny::plotOutput(ns("scatter_plot"), height = 360))
        )
      )
    )
  )
}

mod_eda_server <- function(id, data) {
  stopifnot(is.reactive(data))

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    req_df <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        shiny::validate(shiny::need(FALSE, "No data available."))
      }
      df
    }

    colinfo <- shiny::reactive({
      df <- data()
      req_df(df)
      tibble::tibble(
        name = names(df),
        is_num = purrr::map_lgl(df, is.numeric),
        is_fac = purrr::map_lgl(df, function(x) {
          is.factor(x) || is.character(x) || is.logical(x)
        })
      )
    })

    output$var_picker <- shiny::renderUI({
      ci <- colinfo()
      req(ci)
      shiny::selectInput(
        ns("var"),
        "Variable",
        choices = ci$name,
        selected = ci$name[1]
      )
    })

    # Distribution controls depend on selected variable type
    output$dist_controls <- shiny::renderUI({
      df <- data()
      req(df, input$var)
      x <- df[[input$var]]
      if (is.numeric(x)) {
        shiny::tagList(
          shiny::sliderInput(
            ns("bins"),
            "Histogram bins",
            min = 5,
            max = 100,
            value = 30
          ),
          shiny::checkboxInput(ns("show_density"), "Overlay density", TRUE)
        )
      } else {
        shiny::tagList(
          shiny::numericInput(
            ns("top_n"),
            "Show top N levels",
            value = 20,
            min = 1
          )
        )
      }
    })

    # Distribution plot
    output$dist_plot <- shiny::renderPlot({
      df <- data()
      req(df, input$var)
      x <- df[[input$var]]
      if (is.numeric(x)) {
        dfn <- tibble::tibble(x = x) |> dplyr::filter(!is.na(x))
        p <- ggplot2::ggplot(dfn, ggplot2::aes(x = x)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ..density..),
            bins = input$bins %||% 30,
            alpha = 0.8
          )
        if (isTRUE(input$show_density)) {
          p <- p + ggplot2::geom_density()
        }
        p +
          ggplot2::labs(x = input$var, y = "Density") +
          ggplot2::theme_minimal()
      } else {
        dfc <- tibble::tibble(x = as.character(df[[input$var]])) |>
          dplyr::mutate(x = dplyr::if_else(is.na(x), "<NA>", x)) |>
          dplyr::count(x, sort = TRUE) |>
          dplyr::slice(1:(input$top_n %||% 20))
        ggplot2::ggplot(dfc, ggplot2::aes(x = stats::reorder(x, n), y = n)) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = "Count") +
          ggplot2::theme_minimal()
      }
    })

    # Summary table for selected variable
    output$var_summary <- shiny::renderTable(
      {
        df <- data()
        req(df, input$var)
        x <- df[[input$var]]
        if (is.numeric(x)) {
          x2 <- x[!is.na(x)]
          out <- data.frame(
            n = length(x),
            n_missing = sum(is.na(x)),
            mean = mean(x2),
            sd = stats::sd(x2),
            min = min(x2),
            q25 = stats::quantile(x2, 0.25),
            median = stats::median(x2),
            q75 = stats::quantile(x2, 0.75),
            max = max(x2)
          )
          out[] <- lapply(out, function(v) {
            if (is.numeric(v)) round(v, 4) else v
          })
          out
        } else {
          dfc <- tibble::tibble(x = as.character(x)) |>
            dplyr::mutate(x = dplyr::if_else(is.na(x), "<NA>", x)) |>
            dplyr::count(x, sort = TRUE)
          total <- sum(dfc$n)
          dfc$prop <- round(dfc$n / total, 4)
          names(dfc) <- c("level", "n", "prop")
          head(dfc, 50)
        }
      },
      bordered = TRUE,
      striped = TRUE
    )

    # Scatter plot ----------------------------------------------------------
    output$x_picker <- shiny::renderUI({
      ci <- colinfo()
      req(ci)
      choices <- ci$name[ci$is_num]
      shiny::selectInput(
        ns("x"),
        "X (numeric)",
        choices = choices,
        selected = choices[1] %||% NULL
      )
    })

    output$y_picker <- shiny::renderUI({
      ci <- colinfo()
      req(ci)
      choices <- ci$name[ci$is_num]
      shiny::selectInput(
        ns("y"),
        "Y (numeric)",
        choices = choices,
        selected = choices[2] %||% choices[1] %||% NULL
      )
    })

    output$scatter_plot <- shiny::renderPlot({
      df <- data()
      req(df, input$x, input$y)
      shiny::validate(shiny::need(
        is.numeric(df[[input$x]]),
        "X must be numeric"
      ))
      shiny::validate(shiny::need(
        is.numeric(df[[input$y]]),
        "Y must be numeric"
      ))
      dfx <- tibble::tibble(x = df[[input$x]], y = df[[input$y]]) |>
        dplyr::filter(!is.na(x) & !is.na(y))
      p <- ggplot2::ggplot(dfx, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = input$alpha %||% 0.6) +
        ggplot2::labs(x = input$x, y = input$y) +
        ggplot2::theme_minimal()
      if (isTRUE(input$add_smoother)) {
        p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE)
      }
      p
    })
  })
}
