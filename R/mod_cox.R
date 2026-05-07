# R/mod_cox.R

cox_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::card(
    full_screen = TRUE,

    bslib::card_header(
      "Cox PH Model"
    ),

    shiny::plotOutput(
      ns("cox_plot"),
      height = "600px"
    )
  )
}


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
