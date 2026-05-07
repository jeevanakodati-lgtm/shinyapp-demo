#' Histogram Module - UI
#'
#' @description
#' Creates the user interface for the Histogram module. Users can select a
#' parameter, one or more visits, and treatments to view the distribution of
#' values. Includes a camera button to download the plot as a PNG image.
#'
#' @param id Module namespace ID
#'
#' @return A `bslib::card` containing the histogram controls and plot output
histogram_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::span("Histogram"),
      screenshot_button_ui(
        target_id = ns("hist_plot"),
        filename = "histogram_plot.png"
      )
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
      ns("hist_plot"),
      height = "700px"
    )
  )
}

#' Histogram Module - Server
#'
#' @description
#' Server function for the Histogram module. Handles dynamic filtering of
#' parameters, visits, and treatments, then renders a faceted histogram using
#' ggplot2.
#'
#' @param id Module namespace ID
#' @param data Reactive data frame containing the source dataset
#' @param value_var Variable name for the numeric values (default: "AVAL")
#' @param param_var Variable name for the parameter (default: "PARAM")
#' @param visit_var Variable name for the visit (default: "AVISIT")
#' @param trt_var Variable name for treatment (default: "TRTA")
#' @param title Title prefix for the plot (default: "Histogram")
histogram_server <- function(
    id,
    data,
    value_var = "AVAL",
    param_var = "PARAM",
    visit_var = "AVISIT",
    trt_var = "TRTA",
    title = "Histogram"
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
      shiny::req(
        input$param,
        input$visit,
        input$trt
      )

      data() |>
        dplyr::filter(
          .data[[param_var]] == input$param,
          .data[[visit_var]] %in% input$visit,
          .data[[trt_var]] %in% input$trt,
          !is.na(.data[[value_var]])
        )
    })

    output$hist_plot <- shiny::renderPlot({
      df <- filtered_data()

      ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = .data[[value_var]],
          fill = .data[[trt_var]]
        )
      ) +
        ggplot2::geom_histogram(
          bins = 30,
          alpha = 0.75,
          color = "white"
        ) +
        ggplot2::facet_wrap(
          stats::as.formula(
            paste("~", visit_var)
          )
        ) +
        ggplot2::labs(
          title = paste(title, "of", input$param),
          x = value_var,
          y = "Count"
        ) +
        custom_ggplot_theme(
          treatment_scale = "fill"
        )
    })

  })

}
