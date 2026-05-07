
boxplot_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::card(
    full_screen = TRUE,

    bslib::card_header(
      "Box Plot"
    ),

    bslib::layout_column_wrap(
      width = 1 / 3,

      shiny::selectInput(
        ns("param"),
        "Parameter",
        choices = NULL
      ),

      shiny::selectizeInput(
        ns("visit"),
        "Visits",
        choices = NULL,
        multiple = TRUE
      ),

      shiny::selectizeInput(
        ns("trt"),
        "Treatment",
        choices = NULL,
        multiple = TRUE
      )
    ),

    shiny::plotOutput(
      ns("plot"),
      height = "700px"
    )
  )
}


boxplot_server <- function(
    id,
    data,
    value_var = "AVAL",
    param_var = "PARAM",
    visit_var = "AVISIT",
    trt_var = "TRTA",
    title = "Box Plot"
) {

  shiny::moduleServer(id, function(input, output, session) {

    observe({
      df <- data()

      param_choices <- sort(
        unique(df[[param_var]])
      )

      shiny::updateSelectInput(
        session,
        "param",
        choices = param_choices,
        selected = param_choices[1]
      )
    })

    observeEvent(input$param, {
      df <- data()

      visit_choices <- df |>
        dplyr::filter(
          .data[[param_var]] == input$param
        ) |>
        dplyr::pull(
          .data[[visit_var]]
        ) |>
        unique() |>
        sort()

      shiny::updateSelectizeInput(
        session,
        "visit",
        choices = visit_choices,
        selected = head(visit_choices, 4),
        server = TRUE
      )
    })

    observe({
      df <- data()

      trt_choices <- sort(
        unique(df[[trt_var]])
      )

      shiny::updateSelectizeInput(
        session,
        "trt",
        choices = trt_choices,
        selected = trt_choices,
        server = TRUE
      )
    })

    filtered_data <- shiny::reactive({
      shiny::req(input$param,input$visit,input$trt)

      data() |>
        dplyr::filter(
          .data[[param_var]] == input$param,
          .data[[visit_var]] %in% input$visit,
          .data[[trt_var]] %in% input$trt,
          !is.na(.data[[value_var]])
        )
    })

    output$plot <- shiny::renderPlot({
      df <- filtered_data()

      ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = .data[[trt_var]],
          y = .data[[value_var]],
          fill = .data[[trt_var]]
        )
      ) +
        ggplot2::geom_boxplot(
          alpha = 0.75,
          outlier.alpha = 0.5
        ) +
        ggplot2::facet_wrap(
          stats::as.formula(
            paste("~", visit_var)
          )
        ) +
        ggplot2::labs(
          title = paste(title, "-", input$param),
          x = "Treatment",
          y = value_var
        ) +
        custom_ggplot_theme(
          treatment_scale = "fill"
        )
    })

  })

}
