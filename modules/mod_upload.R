mod_upload_ui <- function(id) {
  ns <- NS(id)
  fileInput(ns("file"), "Choose CSV")
}

mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$file)
      read.csv(input$file$datapath)
    })
  })
}
