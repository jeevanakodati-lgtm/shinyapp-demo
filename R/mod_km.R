# R/mod_km.R

km_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::card(
    full_screen = TRUE,

    bslib::card_header(
      "Kaplan Meier Curve"
    ),

    shiny::plotOutput(
      ns("km_plot"),
      height = "750px"
    )
  )
}


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

      fit |>
        ggsurvfit::ggsurvfit() +
        ggsurvfit::add_risktable() +
        ggplot2::labs(
          title = "",
          x = "Time",
          y = "Survival Probability"
        ) +
        custom_ggplot_theme(
          treatment_scale = "color"
        )
    })

  })

}
