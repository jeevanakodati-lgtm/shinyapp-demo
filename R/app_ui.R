# R/app_ui.R

app_ui <- function(request) {

  bslib::page_navbar(

    title = "Jeevana Kodati's Demo",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2C3E50"
    ),
    shiny::includeCSS("www/css/style.css"),
    bslib::nav_panel(
      title = "Home",
      home_ui("home")
    ),

    bslib::nav_panel(
      title = "Demographics",
      demographics_ui("demographics")
    ),

    bslib::nav_menu(
      title = "Time to Event",
      bslib::nav_panel(
        title = "Kaplan Meier",
        km_ui("km")
      ),
      bslib::nav_panel(
        title = "Cox PH",
        cox_ui("cox")
      )
    ),

    bslib::nav_menu(
      title = "Vital Signs",
      bslib::nav_panel(
        title = "Mean Trend",
        meantrend_ui("vs_trend")
      ),
      bslib::nav_panel(
        title = "Histogram",
        histogram_ui("vs_hist")
      ),

      bslib::nav_panel(
        title = "Box Plot",
        boxplot_ui("vs_box")
      )
    )
  )
}
