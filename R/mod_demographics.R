#' Baseline Demographics Module - UI
#'
#' @description
#' UI for the Baseline Demographics module. Displays a summary table of
#' key demographic characteristics (Age, Sex, Race) by treatment group.
#' Includes a screenshot button to export the table as an image.
#'
#' @param id Module namespace ID
#'
#' @return A bslib card containing the demographics summary table
demographics_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::span("Baseline Demographics"),
      screenshot_button_ui(
        target_id = ns("demo_table"),
        filename = "baseline_demographics.png"
      )
    ),
    gt::gt_output(ns("demo_table"))
  )
}

#' Baseline Demographics Module - Server
#'
#' @description
#' Server logic for the Baseline Demographics module. Generates a summary
#' table using gtsummary and renders it with gt.
#'
#' @param id Module namespace ID
#' @param data A reactive data frame containing the demographics dataset
demographics_server <- function(id, data) {

  shiny::moduleServer(id, function(input, output, session) {

    output$demo_table <- gt::render_gt({

      df <- data()

      # DEMOGRAPHICS TABLE
      gtsummary::tbl_summary(
        data = df,
        by = TRT01A,
        include = c(
          AGE,
          SEX,
          RACE
        ),
        statistic = list(
          AGE ~ "{mean} ({sd})"
        ),
        digits = list(
          AGE ~ 1
        )) |>
        gtsummary::add_overall() |>
        gtsummary::bold_labels() |>
        gtsummary::as_gt()

    })

  })
}
