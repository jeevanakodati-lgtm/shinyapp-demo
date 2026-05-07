# R/app_ui.R

#' App User Interface
#'
#' @description
#' Main UI function for the Shiny application. Builds the navigation structure
#' using `bslib::page_navbar()` and includes all modules. Also sets up
#' resource paths, external JavaScript libraries (html2canvas), and custom CSS.
#'
#' @param request Internal Shiny request object (used for bookmarking/shinyapps.io)
#'
#' @return A complete Shiny UI definition (bslib page_navbar)
app_ui <- function(request) {

  # Serve static files from inst/www (required for JS and CSS when building as package)
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("www", package = "shinyappDemo")
  )

  bslib::page_navbar(
    title = "Jeevana Kodati's Demo",

    # JavaScript and CSS dependencies
    shiny::tags$head(
      # html2canvas for screenshot functionality
      shiny::tags$script(
        src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"
      ),
      shiny::tags$script(src = "www/js/screenshot.js")
    ),

    shiny::includeCSS(
      system.file("www/css/style.css", package = "shinyappDemo")
    ),

    # Theme configuration
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2C3E50"
    ),

    # Navigation panels
    bslib::nav_panel(
      title = "Home",
      home_ui("home")
    ),

    bslib::nav_panel(
      title = "Demographics",
      demographics_ui("demographics")
    ),

    # Time to Event menu
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

    # Vital Signs menu
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
    ),

    # Miscellaneous menu
    bslib::nav_menu(
      title = "Miscellaneous",
      bslib::nav_panel(
        title = "PowerPoint Export",
        misc_ui("misc")
      )
    )
  )
}
