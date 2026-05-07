# R/app_server.R

#' App Server
#'
#' @description
#' Main server function for the Shiny application. This is where all data is
#' loaded and all modules are called. It serves as the central hub connecting
#' the UI with the various analysis modules.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
app_server <- function(input, output, session) {

  ########################################
  # LOAD DATA
  ########################################

  # Load the prepared ADaM datasets from the package
  rds_path <- system.file(
    "extdata",
    "adam.rds",
    package = "shinyappDemo"
  )

  adam_data <- readRDS(rds_path)

  # Clean ADVS data - remove baseline and invalid visits
  advs_clean <- adam_data$advs[
    !is.na(adam_data$advs$AVISITN) &
      adam_data$advs$AVISITN > 0,
  ]

  # Reactive values to hold the datasets (makes them available to all modules)
  rv <- shiny::reactiveValues(
    adsl = adam_data$adsl,
    adtte = adam_data$adtte,
    advs = advs_clean
  )

  ########################################
  # MODULES
  ########################################

  # Home page (currently static - no server logic needed)
  # home_server("home")

  # Demographics Module
  demographics_server(
    id = "demographics",
    data = reactive(rv$adsl)
  )

  # Time to Event Modules
  km_server(
    id = "km",
    data = reactive(rv$adtte)
  )

  cox_server(
    id = "cox",
    data = reactive(rv$adtte)
  )

  ########################################
  # VITAL SIGNS MODULES
  ########################################

  histogram_server(
    id = "vs_hist",
    data = reactive(rv$advs)
  )

  boxplot_server(
    id = "vs_box",
    data = reactive(rv$advs)
  )

  meantrend_server(
    id = "vs_trend",
    data = reactive(rv$advs)
  )

  # Miscellaneous Module (PowerPoint Export)
  misc_server("misc")

}
