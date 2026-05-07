# R/mod_meantrend.R

meantrend_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    full_screen = TRUE,

    bslib::card_header(
      "Mean Trend Plot"
    ),

    bslib::layout_column_wrap(
      width = 1 / 3,

      shiny::selectInput(
        ns("param"),
        "Parameter",
        choices = NULL
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


meantrend_server <- function(
    id,
    data,
    value_var = "AVAL",
    param_var = "PARAM",
    visit_var = "AVISIT",
    visit_num_var = "AVISITN",
    trt_var = "TRTA",
    title = "Mean Trend Plot"
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
      shiny::req(input$param, input$trt)

      data() |>
        dplyr::filter(
          .data[[param_var]] == input$param,
          .data[[trt_var]] %in% input$trt,
          !is.na(.data[[value_var]]),
          !is.na(.data[[visit_var]]),
          !is.na(.data[[visit_num_var]])
        )
    })

    summary_data <- shiny::reactive({
      filtered_data() |>
        dplyr::group_by(
          .data[[visit_var]],
          .data[[visit_num_var]],
          .data[[trt_var]]
        ) |>
        dplyr::summarise(
          Mean = mean(
            .data[[value_var]],
            na.rm = TRUE
          ),
          .groups = "drop"
        ) |>
        dplyr::arrange(
          .data[[visit_num_var]]
        )
    })

    output$plot <- shiny::renderPlot({
      df <- summary_data()

      visit_levels <- df |>
        dplyr::arrange(.data[[visit_num_var]]) |>
        dplyr::pull(.data[[visit_var]]) |>
        unique()

      df[[visit_var]] <- factor(
        df[[visit_var]],
        levels = visit_levels
      )

      ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = .data[[visit_var]],
          y = Mean,
          color = .data[[trt_var]],
          group = .data[[trt_var]]
        )
      ) +
        ggplot2::geom_line(
          linewidth = 1.2
        ) +
        ggplot2::geom_point(
          size = 3
        ) +
        ggplot2::labs(
          title = paste(title, "-", input$param),
          x = "Visit",
          y = paste("Mean", value_var)
        ) +
        custom_ggplot_theme(
          treatment_scale = "color"
        )
    })

  })
}
