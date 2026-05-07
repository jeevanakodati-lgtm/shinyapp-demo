

demographics_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      "Baseline Demographics"
    ),
    gt::gt_output(ns("demo_table"))
  )
}


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
