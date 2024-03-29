#' Advantage plot
#'
#' Generate a plot of scaled evaluations (advantages).
#'
#' @details Given a vector of positional evaluations (in centipawns), the
#'   results are scaled using a form of
#'   [logistic scaling function](https://lichess.org/blog/WFvLpiQAACMA8e9D/learn-from-your-mistakes)
#'   (2 / (1 + exp(-0.004 * scores)) - 1) borrowed from
#'   [lichess.org](lichess.org). This function downplays the impact of
#'   less-than-perfect play when one side is far ahead. The idea being that the
#'   player who is ahead may avoid moves that lead to material gain if those
#'   moves lead to complicated tactics. In such positions players often go for
#'   simple and solid moves that maintain a clear advantage. These types of
#'   moves aren't really blunders. Similarly, a player in a lost position may
#'   make moves designed to complicate the position and create chances for a
#'   comeback, even if the move objectively loses material with perfect play by
#'   the opponent. The scaled values are plotted vs. the number of half-moves.
#'
#' @param scores A numeric vector of positional evaluations (in centipawns).
#' @param style (Default = 'graph') A single-element character vector
#'   indicating the plot style. Allowed values are 'graph' for a traditional
#'   graph with axes, or 'infographic' to add a background gradient and remove
#'   the axes (similar to [lichess.org](lichess.org)).
#' @param scaling (Default = 'lichess') A single-element character vector
#'   indicating how the plot should be scaled. Allowed values are 'none' to plot
#'   the data as is, 'lichess' to apply the same scaling function used by
#'   lichess.org, or 'regan' to apply the scaling function developed by Prof.
#'   Kenneth Regan at the University of Buffalo.
#'
#' @return A ggplot object of the plotted data.
#' @export
#'
#' @seealso
#'   * [rbitr::time_plot()] to plot move time data.
#'   * [rbitr::game_summary_plot()] to plot move time and advantage data with a
#'     table of game stats.
#'
#' @examples
#'   scores <- c(15, 5, 29, -94, 67, 76, 154, -31, 1000, 1000)
#'   advantage_plot(scores)
advantage_plot <- function(scores, style = 'graph', scaling = 'lichess') {
  # Validate input
  assertthat::assert_that(is.numeric(scores))
  assertthat::assert_that(style == 'graph' |
                          style == 'infographic')
  assertthat::assert_that(scaling == 'none' | scaling == 'lichess' |
                          scaling == 'regan')
  # Make the 2 color area plot
  if (scaling == 'none') {
    scaled_scores <- scores
    y_label <- 'Advantage (Centipawns)'
  } else if (scaling == 'lichess') {
    scaled_scores <- winning_chances(scores)
    y_label <- 'Advantage (Lichess Scaling)'
  } else if (scaling == 'regan') {
    scaled_scores <- regan_antiderivative(scores)
    y_label <- 'Advantage (Regan Scaling)'
  }
  n_ply <- length(scores)
  score_frame <- data.frame(
    ply    = 1:n_ply,
    scores = scaled_scores
  )
  if (style == 'infographic') {
    background <- 'gradient'
    p_theme <- ggplot2::theme_void()
  } else if (style == 'graph') {
    background <- 'none'
    p_theme <- ggplot2::geom_blank()
  }
  y_extreme <- max(abs(scaled_scores))
  plot_2_color_area(score_frame, 'ply', 'scores', background = background) +
    ggplot2::geom_point(data    = data.frame(x = c(1, 1), y = c(-y_extreme, y_extreme)),
                        mapping = ggplot2::aes(x = .data$x, y = .data$y),
                        alpha   = 0) +
    ggplot2::geom_segment(ggplot2::aes(x = 1, y = 0, xend = n_ply, yend = 0),
                          color = grDevices::rgb(160, 160, 160,
                                                 maxColorValue = 255)) +
    ggplot2::geom_line(data    = score_frame,
                       mapping = ggplot2::aes(x = .data$ply, y = .data$scores),
                       color   = grDevices::rgb(216, 80, 0,
                                                maxColorValue = 255)) +
    ggplot2::geom_point(data    = score_frame,
                        mapping = ggplot2::aes(x = .data$ply, y = .data$scores),
                        color   = grDevices::rgb(216, 80, 0,
                                                 maxColorValue = 255),
                        size    = 1) +
    ggplot2::xlab('Position') +
    ggplot2::ylab(y_label) +
    ggplot2::scale_x_continuous(
      breaks = function(x) unique(floor(pretty(x, 20))),
      limits = c(1, n_ply)) +
    p_theme +
    ggplot2::theme(legend.position = 'none')

}
