pacman::p_load(shiny, highcharter, bslib)


source("modules/mod_upload.R")
source("modules/mod_cor.R")

ui <- bslib::page_sidebar(
  theme = bs_theme(), # optional theming
  sidebar = sidebar(
    h4("Data"),
    mod_upload_ui("u1"),
    open = "open", # "open", "closed", or "always"
    collapsible = TRUE, # <-- this adds the minimize/toggle
    width = 300
  ),
  tabsetPanel(
    tabPanel("Correlation", mod_cor_ui("c1"))
  )
)

server <- function(input, output, session) {
  d <- mod_upload_server("u1")
  mod_cor_server("c1", d)
}

shiny::shinyApp(ui, server)
