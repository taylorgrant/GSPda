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
      ),

      # UI: t-test -------------------------------------------------------------
      shiny::tabPanel(
        "t-test",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::radioButtons(
              ns("ttest_mode"),
              "Test type",
              choices = c(
                "Two-sample (by group)" = "two_sample",
                "Paired (two columns)" = "paired",
                "One-sample (mean)" = "one_sample"
              ),
              selected = "paired"
            ),
            shiny::uiOutput(ns("ttest_controls")),
            shiny::selectInput(
              ns("ttest_alt"),
              "Alternative hypothesis",
              choices = c("two.sided", "less", "greater"),
              selected = "two.sided"
            ),
            shiny::numericInput(
              ns("ttest_conf"),
              "Confidence level",
              value = 0.95,
              min = 0.5,
              max = 0.999,
              step = 0.01
            ),
            shiny::checkboxInput(
              ns("ttest_na_rm"),
              "Remove missing values",
              TRUE
            )
          ),
          shiny::column(
            8,
            shiny::h5("Results"),
            shiny::verbatimTextOutput(ns('ttest_print'))
          )
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
    # -------------------------- t-test logic -------------------------------
    output$ttest_controls <- shiny::renderUI({
      ci <- colinfo()
      req(ci)
      num_choices <- ci$name[ci$is_num]
      fac_choices <- ci$name[ci$is_fac]

      switch(
        input$ttest_mode %||% "two_sample",
        "two_sample" = shiny::tagList(
          shiny::selectInput(
            ns("ttest_y"),
            "Outcome (numeric)",
            choices = num_choices
          ),
          shiny::selectInput(
            ns("ttest_group"),
            "Group (2 levels)",
            choices = fac_choices
          ),
          shiny::checkboxInput(
            ns("ttest_var_equal"),
            "Assume equal variances",
            FALSE
          )
        ),
        "paired" = shiny::tagList(
          shiny::selectInput(
            ns("ttest_a"),
            "Column A (numeric)",
            choices = num_choices
          ),
          shiny::selectInput(
            ns("ttest_b"),
            "Column B (numeric)",
            choices = num_choices,
            selected = num_choices[2] %||% num_choices[1]
          )
        ),
        "one_sample" = shiny::tagList(
          shiny::selectInput(
            ns("ttest_y1"),
            "Variable (numeric)",
            choices = num_choices
          ),
          shiny::numericInput(
            ns("ttest_mu"),
            "Null mean (μ₀)",
            value = 0,
            step = 0.1
          )
        )
      )
    })

    ttest_result <- shiny::reactive({
      df <- data()
      req(df, input$ttest_mode, input$ttest_alt, input$ttest_conf)

      mode <- input$ttest_mode
      alt <- input$ttest_alt
      conf <- input$ttest_conf

      if (mode == "two_sample") {
        req(input$ttest_y, input$ttest_group)
        y <- df[[input$ttest_y]]
        g <- as.factor(df[[input$ttest_group]])
        d <- tibble::tibble(y = y, g = g)
        if (isTRUE(input$ttest_na_rm)) {
          d <- d |> dplyr::filter(!is.na(y) & !is.na(g))
        }
        shiny::validate(shiny::need(
          nlevels(d$g) == 2,
          "Group must have exactly 2 levels after NA handling."
        ))
        res <- stats::t.test(
          y ~ g,
          data = d,
          alternative = alt,
          var.equal = isTRUE(input$ttest_var_equal),
          conf.level = conf
        )
        list(mode = mode, res = res, data = d)
      } else if (mode == "paired") {
        req(input$ttest_a, input$ttest_b)
        a <- df[[input$ttest_a]]
        b <- df[[input$ttest_b]]
        d <- tibble::tibble(a = a, b = b)
        if (isTRUE(input$ttest_na_rm)) {
          d <- d |> dplyr::filter(!is.na(a) & !is.na(b))
        }
        shiny::validate(shiny::need(
          nrow(d) > 1,
          "Not enough paired observations after NA handling."
        ))
        res <- stats::t.test(
          d$a,
          d$b,
          paired = TRUE,
          alternative = alt,
          conf.level = conf
        )
        list(mode = mode, res = res, data = d)
      } else {
        # one_sample
        req(input$ttest_y1, input$ttest_mu)
        y <- df[[input$ttest_y1]]
        if (isTRUE(input$ttest_na_rm)) {
          y <- y[!is.na(y)]
        }
        shiny::validate(shiny::need(
          length(y) > 1,
          "Not enough observations after NA handling."
        ))
        res <- stats::t.test(
          y,
          mu = input$ttest_mu,
          alternative = alt,
          conf.level = conf
        )
        list(mode = mode, res = res, data = tibble::tibble(y = y))
      }
    })

    output$ttest_print <- shiny::renderPrint({
      tr <- ttest_result()
      req(tr)
      print(tr$res)
      if (tr$mode == "paired") {
        cat("\nPaired column means:\n")
        with(
          tr$data,
          cat(
            "Mean(A) =",
            mean(a, na.rm = TRUE),
            " | mean(B) = ",
            mean(b, na.rm = TRUE),
            "\n"
          )
        )
      }
    })
    output$ttest_table <- shiny::renderTable(
      {
        tr <- ttest_result()
        req(tr)
        tt <- tr$res
        est <- tt$estimate
        est_names <- names(est)

        df_out <- data.frame(
          statistic = unname(tt$statistic),
          df = unname(tt$parameter),
          p_value = tt$p.value,
          conf_low = tt$conf.int[1],
          conf_high = tt$conf.int[2],
          alternative = tt$alternative,
          stringsAsFactors = FALSE
        )

        # add estimates columns with safe names
        if (!is.null(est)) {
          for (i in seq_along(est)) {
            nm <- if (!is.null(est_names)) {
              est_names[i]
            } else {
              paste0("estimate_", i)
            }
            df_out[[nm]] <- est[[i]]
          }
        }
        as.data.frame(lapply(df_out, function(v) {
          if (is.numeric(v)) round(v, 5) else v
        }))
      },
      bordered = TRUE,
      striped = TRUE
    )

    output$ttest_plot <- shiny::renderPlot({
      tr <- ttest_result()
      req(tr)
      mode <- tr$mode
      tt <- tr$res

      if (mode == "two_sample") {
        d <- tr$data
        ggplot2::ggplot(d, ggplot2::aes(x = g, y = y)) +
          ggplot2::geom_boxplot(alpha = 0.7) +
          ggplot2::geom_jitter(width = 0.1, alpha = 0.5) +
          ggplot2::labs(
            x = names(d)[2],
            y = names(d)[1],
            subtitle = paste0("p = ", signif(tt$p.value, 4))
          ) +
          ggplot2::theme_minimal()
      } else if (mode == "paired") {
        d <- tr$data
        d$diff <- d$a - d$b
        ggplot2::ggplot(d, ggplot2::aes(x = diff)) +
          ggplot2::geom_histogram(bins = 30, alpha = 0.8) +
          ggplot2::geom_density() +
          ggplot2::geom_vline(
            xintercept = mean(d$diff, na.rm = TRUE),
            linetype = "dashed"
          ) +
          ggplot2::labs(
            x = "Difference (A - B)",
            subtitle = paste0("p = ", signif(tt$p.value, 4))
          ) +
          ggplot2::theme_minimal()
      } else {
        d <- tr$data
        ggplot2::ggplot(d, ggplot2::aes(x = y)) +
          ggplot2::geom_histogram(bins = 30, alpha = 0.8) +
          ggplot2::geom_density() +
          ggplot2::geom_vline(
            xintercept = mean(d$y, na.rm = TRUE),
            linetype = "dashed"
          ) +
          ggplot2::geom_vline(
            xintercept = input$ttest_mu %||% 0,
            linetype = "dotted"
          ) +
          ggplot2::labs(
            x = "Values",
            subtitle = paste0(
              "p = ",
              signif(tt$p.value, 4),
              "  |  μ̂ = ",
              round(mean(d$y, na.rm = TRUE), 3),
              "  vs  μ₀ = ",
              input$ttest_mu %||% 0
            )
          ) +
          ggplot2::theme_minimal()
      }
    })
  })
}
