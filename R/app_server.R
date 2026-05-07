# R/app_server.R

app_server <- function(input, output, session) {

  ########################################
  # LOAD DATA
  ########################################

  rds_path <- system.file(
    "extdata",
    "adam.rds",
    package = "shinyappDemo"
  )

  adam_data <- readRDS(rds_path)

  advs_clean <- adam_data$advs[
    !is.na(adam_data$advs$AVISITN) &
      adam_data$advs$AVISITN > 0,
  ]

  rv <- shiny::reactiveValues(
    adsl  = adam_data$adsl,
    adtte = adam_data$adtte,
    advs  = advs_clean
  )

  ########################################
  # MODULES
  ########################################

  home_server("home")

  demographics_server(
    id = "demographics",
    data = reactive(rv$adsl)
  )

  km_server(
    id = "km",
    data = reactive(rv$adtte)
  )

  cox_server(
    id = "cox",
    data = reactive(rv$adtte)
  )

  ########################################
  # VITAL SIGNS
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

}
