# R/mod_home.R

home_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::layout_column_wrap(

    width = 1/2,

    bslib::card(

      bslib::card_header("Overview"),

      shiny::p("Home module")
    )
  )
}

home_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

  })
}
