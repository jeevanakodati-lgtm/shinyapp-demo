#' Screenshot Button UI Helper
#'
#' @description
#' Creates a reusable screenshot button with a camera icon. When clicked,
#' it triggers the JavaScript function to capture the target element as a PNG
#' image using html2canvas.
#'
#' @param target_id The ID of the HTML element to capture (usually a plot or card).
#' @param filename The default filename for the downloaded screenshot
#'   (should end with `.png`).
#'
#' @return A Shiny `tags$button` element with proper data attributes for JavaScript.
#'
#' @examples
#' screenshot_button_ui(
#'   target_id = "my_plot",
#'   filename = "mean_trend_plot.png"
#' )
screenshot_button_ui <- function(target_id, filename = "plot.png") {
  shiny::tags$button(
    type = "button",
    class = "btn btn-sm btn-outline-primary screenshot-btn",
    `data-target` = target_id,
    `data-filename` = filename,
    shiny::icon("camera"),
    " Screenshot",
    style = "margin-left: auto;"
  )
}
