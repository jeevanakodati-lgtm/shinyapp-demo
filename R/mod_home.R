#R/mod_home.R

#' Home Page Module - UI
#'
#' @description
#' This is the landing/home page of the application. It provides an
#' overview of the app, explains its purpose, describes the datasets used,
#' and lists all available modules.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI element (div) containing the welcome page content
home_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::div(
    class = "home-page",

    shiny::h2("Welcome to the App"),

    shiny::p(
      "This Shiny application is built purely for demonstration purposes. ",
      "It shows a modular clinical dashboard structure using bslib, reusable plotting modules, ",
      "and CDISC-style analysis datasets."
    ),

    shiny::h4("Datasets Used"),

    shiny::p(
      "The app uses CDISC Pilot 3 style datasets including ADSL, ADTTE, and ADVS. ",
      "These datasets are commonly used for demonstrating clinical reporting workflows, ",
      "including demographics, time-to-event analysis, and vital signs visualisations."
    ),

    shiny::tags$a(
      href = "https://github.com/RConsortium/submissions-pilot3-adam",
      target = "_blank",
      "View CDISC Pilot 3 ADaM GitHub Repository"
    ),

    shiny::h4("Available Modules"),

    shiny::tags$ul(
      shiny::tags$li(
        shiny::strong("Demographics: "),
        "Displays a baseline demographic summary grouped by treatment."
      ),
      shiny::tags$li(
        shiny::strong("Time to Event: "),
        "Includes Kaplan Meier and Cox PH modules for survival analysis."
      ),
      shiny::tags$li(
        shiny::strong("Vital Signs: "),
        "Includes mean trend, histogram, and box plot modules for exploring ADVS data."
      )
    ),

    shiny::h4("Miscellaneous Module"),

    shiny::p(
      "The Miscellaneous tab demonstrates PowerPoint viewing and export workflows. ",
      "This includes functionality to preview generated outputs and export selected plots ",
      "or reports into presentation-ready PowerPoint files."
    )
  )
}


