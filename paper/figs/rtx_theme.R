# Shared figure theme. R-first by workspace convention; do not add a
# matplotlib path to this tree.

rtx_theme <- function(base_size = 9) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(linewidth = 0.3),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
}
