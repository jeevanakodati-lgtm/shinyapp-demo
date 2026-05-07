#' Cox PH Model Module - UI
#'
#' @description
#' UI for the Cox Proportional Hazards model module. Displays a forest plot
#' showing the hazard ratios from the Cox model. Includes a screenshot button
#' to export the plot as PNG.
#'
#' @param id Module namespace ID
#'
#' @return A bslib card containing the Cox PH forest plot
cox_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::span("Cox PH Model"),
      screenshot_button_ui(
        target_id = ns("cox_plot"),
        filename = "cox_ph_plot.png"
      )
    ),
    shiny::plotOutput(
      ns("cox_plot"),
      height = "600px"
    )
  )
}

#' Cox PH Model Module - Server
#'
#' @description
#' Server logic for the Cox Proportional Hazards module. Fits a Cox PH model
#' using the survival package and renders a forest plot using survminer.
#'
#' @param id Module namespace ID
#' @param data A reactive data frame containing the survival analysis dataset
#' @param time_var Name of the time-to-event variable (default: "AVAL")
#' @param censor_var Name of the censoring indicator variable (default: "CNSR")
#' @param trt_var Name of the treatment variable (default: "TRTA")
cox_server <- function(
    id,
    data,
    time_var = "AVAL",
    censor_var = "CNSR",
    trt_var = "TRTA"
) {

  shiny::moduleServer(id, function(input, output, session) {

    output$cox_plot <- shiny::renderPlot({

      df <- data()

      fit <- survival::coxph(
        stats::as.formula(
          paste0(
            "survival::Surv(",
            time_var,
            ", 1 - ",
            censor_var,
            ") ~ ",
            trt_var
          )
        ),
        data = df
      )

      survminer::ggforest(
        fit,
        data = df
      )

    })

  })

}
