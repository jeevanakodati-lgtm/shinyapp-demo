#' Miscellaneous Module - PowerPoint Export UI
#'
#' @description
#' UI module for the PowerPoint Export workflow. Allows users to upload plot
#' images, add a presentation title, preview the images, and download them
#' as a PowerPoint (.pptx) file.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI tag containing the PowerPoint export interface
misc_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::div(
    class = "misc-page",

    shiny::h3("PowerPoint Export Workflow"),

    shiny::p(
      "Upload one or more plot images, preview them, and export them into a PowerPoint file."
    ),

    bslib::layout_columns(
      col_widths = c(4, 8),

      bslib::card(
        full_screen = FALSE,

        bslib::card_header("Upload and Export"),

        shiny::textInput(
          ns("ppt_title"),
          "Presentation Title",
          value = "Clinical Report"
        ),

        shiny::fileInput(
          ns("plot_images"),
          "Upload Plot Images",
          multiple = TRUE,
          accept = c(
            "image/png",
            "image/jpeg",
            ".png",
            ".jpg",
            ".jpeg"
          )
        ),

        shiny::downloadButton(
          ns("download_ppt"),
          "Create PowerPoint"
        )
      ),

      bslib::card(
        full_screen = TRUE,

        bslib::card_header("Uploaded Plot Preview"),

        shiny::uiOutput(
          ns("image_preview")
        )
      )
    )
  )
}

#' Miscellaneous Module - PowerPoint Export Server
#'
#' @description
#' Server module for the PowerPoint export functionality. Handles image upload,
#' preview rendering using base64 encoding, and generates a PowerPoint file
#' @import officer
#'
#' @param id Module namespace ID
#'
#' @return No return value. Sets up Shiny reactive endpoints.
misc_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    output$image_preview <- shiny::renderUI({

      shiny::req(input$plot_images)

      image_tags <- lapply(seq_len(nrow(input$plot_images)), function(i) {

        file_path <- input$plot_images$datapath[i]
        file_name <- input$plot_images$name[i]
        file_type <- input$plot_images$type[i]

        shiny::div(
          class = "uploaded-image-preview",

          shiny::h5(file_name),

          shiny::tags$img(
            src = base64enc::dataURI(
              file = file_path,
              mime = file_type
            ),
            class = "uploaded-image"
          )
        )
      })

      shiny::tagList(image_tags)
    })


    output$download_ppt <- shiny::downloadHandler(

      filename = function() {
        paste0(
          "clinical_report_",
          Sys.Date(),
          ".pptx"
        )
      },

      content = function(file) {

        shiny::req(input$plot_images)

        ppt <- officer::read_pptx()

        ppt <- officer::add_slide(
          ppt,
          layout = "Title Slide",
          master = "Office Theme"
        )

        ppt <- officer::ph_with(
          ppt,
          value = input$ppt_title,
          location = officer::ph_location_type(
            type = "ctrTitle"
          )
        )

        ppt <- officer::ph_with(
          ppt,
          value = paste("Generated on", Sys.Date()),
          location = officer::ph_location_type(
            type = "subTitle"
          )
        )

        for (i in seq_len(nrow(input$plot_images))) {

          ppt <- officer::add_slide(
            ppt,
            layout = "Title and Content",
            master = "Office Theme"
          )

          ppt <- officer::ph_with(
            ppt,
            value = input$plot_images$name[i],
            location = officer::ph_location_type(
              type = "title"
            )
          )

          ppt <- officer::ph_with(
            ppt,
            value = officer::external_img(
              src = input$plot_images$datapath[i],
              width = 8,
              height = 4.8
            ),
            location = officer::ph_location_type(
              type = "body"
            )
          )
        }

        print(
          ppt,
          target = file
        )
      }
    )
  })
}
