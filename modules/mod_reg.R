# Basic OLS Regression Module (v0.1)
# ------------------------------------------------------------
# UI:     mod_lm_ui(id)
# Server: mod_lm_server(id, data)
#
# `data` should be a reactive that returns a data.frame/tibble.
# Start simple: choose a numeric outcome, choose one or more predictors,
# fit OLS with lm(). Show: formula used, coefficients (tidy), fit stats,
# and a basic residuals vs fitted diagnostic.
# ------------------------------------------------------------

mod_reg_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::uiOutput(ns("y_picker")),
        shiny::uiOutput(ns("x_picker")),
        shiny::checkboxInput(
          ns("include_intercept"),
          "Include intercept",
          value = TRUE
        ),
        shiny::actionButton(ns("fit"), "Fit model")
      ),
      shiny::column(
        8,
        shiny::h4("Model formula"),
        shiny::verbatimTextOutput(ns("formula_txt")),
        shiny::h4("Coefficients"),
        shiny::tableOutput(ns("coef_tbl")),
        shiny::h4("Collinearity (VIF)"),
        shiny::tableOutput(ns("vif_tbl")),
        shiny::h4("Fit statistics"),
        shiny::tableOutput(ns("fit_tbl")),
        shiny::h4("Diagnostics"),
        shiny::tabsetPanel(
          id = ns("diag_tabs"),
          type = "tabs",
          shiny::tabPanel(
            "Residuals vs Fitted",
            shiny::plotOutput(ns("diag_plot"), height = 280)
          ),
          shiny::tabPanel(
            "Normal Q–Q",
            shiny::plotOutput(ns("qq_plot"), height = 280)
          ),
          shiny::tabPanel(
            "Leverage/Cook's",
            shiny::plotOutput(ns("cook_plot"), height = 280)
          )
        )
      )
    )
  )
}

mod_reg_server <- function(id, data) {
  stopifnot(is.reactive(data))

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # small helper used below (avoid importing rlang just for this)
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    # Helpers ---------------------------------------------------------------
    numeric_cols <- shiny::reactive({
      df <- data()
      req_df(df)
      names(df)[purrr::map_lgl(df, is.numeric)]
    })

    all_cols <- shiny::reactive({
      df <- data()
      req_df(df)
      names(df)
    })

    req_df <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        shiny::validate(shiny::need(FALSE, "No data available."))
      }
      df
    }

    # Dynamic pickers -------------------------------------------------------
    output$y_picker <- shiny::renderUI({
      df <- data()
      req(df)
      shiny::selectInput(
        ns("y"),
        "Outcome (numeric)",
        choices = numeric_cols(),
        selected = NULL
      )
    })

    output$x_picker <- shiny::renderUI({
      df <- data()
      req(df)
      shiny::selectizeInput(
        ns("x"),
        "Predictors",
        choices = setdiff(all_cols(), input$y %||% character()),
        selected = NULL,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    # VIF table  -------------------------------------------------------------
    # Compute VIF with car::vif when available; fallback to model-matrix approach
    compute_vif <- function(model) {
      vif_out <- NULL
      if (requireNamespace("car", quietly = TRUE)) {
        out <- tryCatch(car::vif(model), error = function(e) NULL)
        if (!is.null(out)) {
          if (is.matrix(out)) {
            # GVIF -> convert to GVIF^(1/(2*Df)) for factors
            Df <- out[, "Df"]
            gv <- out[, "GVIF"]
            adj <- gv^(1 / (2 * Df))
            vif_out <- data.frame(
              term = rownames(out),
              vif = as.numeric(adj),
              row.names = NULL
            )
          } else {
            vif_out <- data.frame(
              term = names(out),
              vif = as.numeric(out),
              row.names = NULL
            )
          }
        }
      }
      if (is.null(vif_out)) {
        # Fallback using model matrix (works broadly; treats dummies separately)
        mm <- stats::model.matrix(model)
        if (ncol(mm) == 0) {
          return(data.frame(term = character(), vif = numeric()))
        }
        if (colnames(mm)[1] == "(Intercept)") {
          mm <- mm[, -1, drop = FALSE]
        }
        if (ncol(mm) < 1) {
          return(data.frame(term = character(), vif = numeric()))
        }
        res <- lapply(seq_len(ncol(mm)), function(j) {
          y <- mm[, j]
          X <- mm[, -j, drop = FALSE]
          if (ncol(X) == 0) {
            return(data.frame(term = colnames(mm)[j], vif = 1))
          }
          r2 <- tryCatch(
            summary(stats::lm(y ~ X))$r.squared,
            error = function(e) NA_real_
          )
          vif <- if (is.na(r2)) NA_real_ else 1 / (1 - r2)
          data.frame(term = colnames(mm)[j], vif = vif)
        })
        vif_out <- do.call(rbind, res)
      }
      vif_out[order(-vif_out$vif), , drop = FALSE]
    }

    # Build formula ---------------------------------------------------------
    fml <- shiny::reactive({
      req(input$y)
      preds <- input$x %||% character()
      rhs <- if (length(preds) == 0) "1" else paste(preds, collapse = " + ")
      if (!isTRUE(input$include_intercept)) {
        rhs <- paste0("0 + ", rhs)
      }
      stats::as.formula(paste(input$y, "~", rhs))
    })

    output$formula_txt <- shiny::renderText({
      req(input$y)
      paste0(deparse(fml()))
    })

    # Fit model -------------------------------------------------------------
    fit <- shiny::eventReactive(
      input$fit,
      {
        df <- data()
        req(df, input$y)

        # Validate types
        if (!is.numeric(df[[input$y]])) {
          shiny::validate(shiny::need(
            FALSE,
            "Outcome must be numeric for OLS."
          ))
        }
        if (!is.null(input$x) && length(input$x) > 0) {
          # Drop rows with missing in used columns for a clean fit
          used <- c(input$y, input$x)
          df <- stats::na.omit(df[, used, drop = FALSE])
        } else {
          # If no predictors, just intercept-only model allowed
          df <- stats::na.omit(df[, input$y, drop = FALSE])
        }

        stats::lm(formula = fml(), data = df)
      },
      ignoreInit = TRUE
    )

    # Outputs ---------------------------------------------------------------
    output$coef_tbl <- shiny::renderTable(
      {
        m <- fit()
        req(m)
        tb <- broom::tidy(m)
        # Round for compactness
        tb$estimate <- round(tb$estimate, 4)
        if ("std.error" %in% names(tb)) {
          tb$std.error <- round(tb$std.error, 4)
        }
        if ("statistic" %in% names(tb)) {
          tb$statistic <- round(tb$statistic, 3)
        }
        if ("p.value" %in% names(tb)) {
          tb$p.value <- signif(tb$p.value, 3)
        }
        tb
      },
      striped = TRUE,
      bordered = TRUE,
      digits = 3
    )

    output$vif_tbl <- shiny::renderTable(
      {
        m <- fit()
        req(m)
        vt <- compute_vif(m)
        if (is.null(vt) || !nrow(vt)) {
          return(data.frame())
        }
        vt$flag <- ifelse(
          is.finite(vt$vif) & vt$vif > 10,
          "HIGH (>10)",
          ifelse(is.finite(vt$vif) & vt$vif > 5, ">5", "")
        )
        vt$vif <- round(vt$vif, 3)
        vt[, c("term", "vif", "flag")]
      },
      striped = TRUE,
      bordered = TRUE
    )

    output$fit_tbl <- shiny::renderTable(
      {
        m <- fit()
        req(m)
        gl <- broom::glance(m)
        out <- data.frame(
          r.squared = round(gl$r.squared, 4),
          adj.r.squared = round(gl$adj.r.squared, 4),
          sigma = round(gl$sigma, 4),
          df = gl$df,
          AIC = round(gl$AIC, 2),
          BIC = round(gl$BIC, 2),
          nobs = gl$nobs
        )
        out
      },
      bordered = TRUE
    )

    output$diag_plot <- shiny::renderPlot({
      m <- fit()
      req(m)
      dfp <- data.frame(
        fitted = stats::fitted(m),
        resid = stats::residuals(m)
      )
      ggplot2::ggplot(dfp, ggplot2::aes(x = fitted, y = resid)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::labs(x = "Fitted values", y = "Residuals") +
        ggplot2::theme_minimal()
    })

    output$qq_plot <- shiny::renderPlot({
      m <- fit()
      req(m)
      dfq <- data.frame(sample = stats::rstandard(m))
      ggplot2::ggplot(dfq, ggplot2::aes(sample = sample)) +
        ggplot2::stat_qq(alpha = 0.7) +
        ggplot2::stat_qq_line(linetype = 2) +
        ggplot2::labs(
          x = "Theoretical Quantiles",
          y = "Standardized Residuals"
        ) +
        ggplot2::theme_minimal()
    })

    output$cook_plot <- shiny::renderPlot({
      m <- fit()
      req(m)
      lev <- stats::hatvalues(m)
      rstd <- stats::rstandard(m)
      cook <- stats::cooks.distance(m)
      n <- stats::nobs(m)
      p <- length(stats::coef(m))
      thr_cook <- 4 / n
      thr_lev <- 2 * p / n
      dfc <- data.frame(
        id = seq_along(lev),
        leverage = as.numeric(lev),
        rstd = as.numeric(rstd),
        cook = as.numeric(cook)
      )
      # choose top points by Cook's for labeling
      top_idx <- head(order(dfc$cook, decreasing = TRUE), 3)
      ggplot2::ggplot(dfc, ggplot2::aes(x = leverage, y = rstd)) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::geom_vline(xintercept = thr_lev, linetype = 3) +
        ggplot2::geom_point(ggplot2::aes(size = cook), alpha = 0.7) +
        ggplot2::geom_text(
          data = dfc[top_idx, ],
          ggplot2::aes(label = id),
          vjust = -0.8,
          size = 3
        ) +
        ggplot2::labs(
          x = paste0("Leverage (p=", p, ", n=", n, ")"),
          y = "Standardized Residuals",
          caption = paste0(
            "Point size ∝ Cook's D; dashed v-line at ~2p/n; highlight labels = top 3 Cook's (threshold ≈ ",
            round(thr_cook, 3),
            ")"
          )
        ) +
        ggplot2::guides(size = "none") +
        ggplot2::theme_minimal()
    })

    # Return the fitted model (reactive) for downstream modules if needed
    return(fit)
  })
}
