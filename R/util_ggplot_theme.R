# R/custom_ggplot_theme.R

custom_ggplot_theme <- function(
    treatment_scale = c("color", "fill", "both", "none"),
    base_size = 14,
    legend_position = "bottom"
) {
  treatment_scale <- match.arg(treatment_scale)

  treatment_colors <- c(
    "Placebo" = "#4E79A7",
    "Xanomeline Low Dose" = "#59A14F",
    "Xanomeline High Dose" = "#E15759"
  )

  theme_obj <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 2
      ),
      axis.title = ggplot2::element_text(
        face = "bold"
      ),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(
        face = "bold"
      ),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (treatment_scale == "color") {
    return(
      list(
        ggplot2::scale_color_manual(
          name = "Treatment",
          values = treatment_colors
        ),
        theme_obj
      )
    )
  }

  if (treatment_scale == "fill") {
    return(
      list(
        ggplot2::scale_fill_manual(
          name = "Treatment",
          values = treatment_colors
        ),
        theme_obj
      )
    )
  }

  if (treatment_scale == "both") {
    return(
      list(
        ggplot2::scale_color_manual(
          name = "Treatment",
          values = treatment_colors
        ),
        ggplot2::scale_fill_manual(
          name = "Treatment",
          values = treatment_colors
        ),
        theme_obj
      )
    )
  }

  theme_obj
}
