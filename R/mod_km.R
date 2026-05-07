# R/mod_km.R

#' Kaplan Meier Curve Module - UI
#'
#' @description
#' UI for the Kaplan Meier module. Displays a survival curve with risk table
#' for time-to-event analysis. The full card is used as the screenshot target
#' so both the plot and the risk table are captured together.
#'
#' @param id Module namespace ID
#'
#' @return A bslib card containing the Kaplan Meier plot and risk table
km_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    id = ns("card"),                    # Important: used for screenshot
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::span("Kaplan Meier Curve"),
      screenshot_button_ui(
        target_id = ns("card"),
        filename = "kaplan_meier_curve.png"
      )
    ),
    shiny::plotOutput(
      ns("km_plot"),
      height = "750px"
    )
  )
}


#' Kaplan Meier Curve Module - Server
#'
#' @description
#' Server logic for the Kaplan Meier module. Fits a survival curve using
#' `survfit()` and renders it with risk table using the ggsurvfit package.
#'
#' @param id Module namespace ID
#' @param data A reactive data frame containing the time-to-event dataset
#' @param time_var Name of the time variable (default: "AVAL")
#' @param censor_var Name of the censoring indicator (default: "CNSR")
#' @param trt_var Name of the treatment variable (default: "TRTA")
km_server <- function(
    id,
    data,
    time_var = "AVAL",
    censor_var = "CNSR",
    trt_var = "TRTA"
) {
  shiny::moduleServer(id, function(input, output, session) {

    output$km_plot <- shiny::renderPlot({
      df <- data()

      # Fit Kaplan-Meier survival curves
      fit <- survival::survfit(
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

      # Custom colors for treatments
      trt_colors <- stats::setNames(
        c("#4E79A7", "#59A14F", "#E15759"),
        c(
          paste0(trt_var, "=Placebo"),
          paste0(trt_var, "=Xanomeline Low Dose"),
          paste0(trt_var, "=Xanomeline High Dose")
        )
      )

      # Render survival plot with risk table
      fit |>
        ggsurvfit::ggsurvfit() +
        ggsurvfit::add_risktable() +
        ggplot2::scale_color_manual(
          values = trt_colors,
          name = "Treatment"
        ) +
        ggplot2::labs(
          title = "",
          x = "Time",
          y = "Survival Probability"
        ) +
        custom_ggplot_theme(treatment_scale = "none")
    })
  })
}
