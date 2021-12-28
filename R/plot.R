#' Plots a standard measurement using ggplot2's ggplot
#'
#' @param data Data that is vizualized
#' @param metric Metric displayed on y axis
#' @param labels Labels of y axis
#' @param ... Passes additional arguments through ggplot
#'
#' @export
plot_std_measurement <- function(data, metric, labels = scales::comma, ...) {
  max_n <- data %>% dplyr::pull(problem_size) %>% max()

  data %>%
    ggplot2::ggplot(ggplot2::aes(problem_size, {{ metric }}, ...)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    hubnR::scale_color_hubnr(palette = "mixed") +
    ggplot2::scale_x_continuous(breaks = seq(0,max_n + 1000,1000)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = "N")
}

#' Adds standard color scheme and geoms to a ggplot object
#'
#' @param ... Incoming plot
#' @param max_n Last break on x axis
#' @param step_x_break Steps between x axis ticks
#'
#' @export
add_std_plot_plot <- function(..., max_n = 16000, step_x_break = 1000) {
  ... +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    hubnR::scale_color_hubnr(palette = "mixed") +
    hubnR::scale_fill_hubnr(palette = "mixed") +
    ggplot2::scale_x_continuous(breaks = seq(0,max_n,step_x_break)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = "N")
}

#' Adds only standard color scheme and axis format to a ggplot object
#'
#' @param ... Incoming plot
#' @param max_n Last break on x axis
#' @param step_x_break Steps between x axis ticks
#'
#' @export
add_std_plot_format <- function(..., max_n = 16000, step_x_break = 1000) {
  ... +
    hubnR::scale_color_hubnr(palette = "mixed") +
    hubnR::scale_fill_hubnr(palette = "mixed") +
    ggplot2::scale_x_continuous(breaks = seq(0,max_n,step_x_break)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = "N")
}
