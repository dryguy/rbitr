#' Lichess advantage plot
#'
#' Generate an advantage plot similar to the ones in the "Computer analysis" tab
#'   on lichess.org.
#'
#' @details Given a vector of positional evaluations (in centipawns), the
#'   results are scaled using the same exponential function used by lichess.org,
#'   and the results are plotted using the same color scheme.
#'
#' @param scores A numeric or integer vector of positional evaluations (in
#'   centipawns.)
#'
#' @return A ggplot object of the plotted data.
#' @export
#'
#' @examples
#'   scores <- c(15, 5, 29, -94, 67, 76, 154, -31, 1000, 1000)
#'   lichess_advantage_plot(scores)
lichess_advantage_plot <- function(scores) {
  # Validate input
  assertthat::assert_that(is.numeric(scores) |
                          is.integer(scores))

  # Make the 2 color area plot
  n_ply <- length(scores)
  score_frame <- data.frame(
    ply    = 1:n_ply,
    scores = winning_chances(scores)
  )
  plot_2_color_area(score_frame, 'ply', 'scores') +
    #ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::geom_point(data    = data.frame(x = c(1, 1), y = c(-1, 1)),
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
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = 'none')

}
