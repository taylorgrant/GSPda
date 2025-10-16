mod_upload_ui <- function(id, label = "Choose CSV or Excel") {
  ns <- shiny::NS(id)
  shiny::fileInput(
    ns("file"),
    label,
    accept = c(
      ".csv",
      ".xlsx",
      ".xls",
      "text/csv",
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      "application/vnd.ms-excel"
    )
  )
}


mod_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # tiny helper
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    reader <- shiny::reactive({
      req(input$file)
      path <- input$file$datapath
      ext <- tolower(tools::file_ext(input$file$name %||% path))

      switch(
        ext,
        csv = function() {
          # quiet, reasonable defaults
          readr::read_csv(path, show_col_types = FALSE, guess_max = 5000) |>
            janitor::clean_names()
        },
        xlsx = function() {
          readxl::read_excel(path, .name_repair = "unique") |>
            janitor::clean_names()
        },
        xls = function() {
          readxl::read_excel(path, .name_repair = "unique") |>
            janitor::clean_names()
        },
        {
          shiny::validate(shiny::need(
            FALSE,
            paste0("Unsupported file type: ", ext)
          ))
        }
      )
    })

    shiny::reactive({
      # execute the chosen reader and return a tibble/data.frame
      reader()()
    })
  })
}
