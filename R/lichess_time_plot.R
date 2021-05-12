#' Lichess time plot
#'
#' #' Generate a move times plot similar to the ones in the "Computer analysis"
#'   tab on lichess.org.
#'
#' @details Given vectors of move times (in seconds), the results are scaled
#'   using the same logarithmic function used by lichess.org, and the results
#'   are plotted using the same color scheme.
#'
#' @param white_move_times A vector of move times for white, in seconds.
#' @param black_move_times A vector of move times for black, in seconds.
#'
#' @return A ggplot object of the plotted data.
#' @export
#'
#' @examples
#'   white_move_times <- c(4, 10, 5, 10)
#'   black_move_times <- c(3, 4, 7)
#'   lichess_time_plot(white_move_times, black_move_times)
lichess_time_plot <- function(white_move_times, black_move_times) {
  # Validate input
  assertthat::assert_that(is.numeric(white_move_times) |
                          is.integer(white_move_times))
  assertthat::assert_that(is.numeric(black_move_times) |
                          is.integer(black_move_times))
  assertthat::assert_that(length(white_move_times) -
                          length(black_move_times) < 2)

  # Create a data frame of move times
  white_move_times <- c(0, scale_move_times(white_move_times))
  black_move_times <- c(0, scale_move_times(black_move_times))
  max_y <- max(c(white_move_times, black_move_times))
  y_offset <- 0.01 * max_y
  y_lim <- 1.4 * max_y
  if (length(black_move_times) < length(white_move_times)) {
    black_move_times <- c(black_move_times, NA)
  }
  n_ply <- length(white_move_times) + length(black_move_times)
  ply <- 1:n_ply
  lichess_move_times <- data.frame(
    white_ply = ply[ply %% 2 == 1] + 0.5,
    black_ply = ply[ply %% 2 == 0] + 0.5,
    white_times = white_move_times,
    black_times = black_move_times
  )

  # Load background gradient
  gradient_path <- file.path(
    system.file(package = 'rbitr'),
    'extdata',
    'gradient.png'
  )
  gradient_background <- png::readPNG(gradient_path)

  # Make the move time plot
  ggplot2::ggplot() +
    ggplot2::annotation_custom(grid::rasterGrob(gradient_background,
                                                width  = grid::unit(1, 'npc'),
                                                height = grid::unit(1, 'npc'))) +
    ggplot2::geom_area(data    = lichess_move_times,
                       mapping = ggplot2::aes(x = .data$white_ply,
                                              y = .data$white_times + y_offset),
                       color   = grDevices::rgb(83,  160, 233,
                                                maxColorValue = 255),
                       fill    = grDevices::rgb(252, 251, 250,
                                                maxColorValue = 255),
                       alpha   = 0.71) +
    ggplot2::geom_point(data    = lichess_move_times,
                        mapping = ggplot2::aes(x = .data$white_ply,
                                               y = .data$white_times + y_offset),
                        color   = grDevices::rgb(83,  160, 233,
                                                 maxColorValue = 255),
                        size    = 1) +
    ggplot2::geom_area(data    = lichess_move_times,
                       mapping = ggplot2::aes(x = .data$black_ply,
                                              y = -.data$black_times - y_offset),
                       fill    = grDevices::rgb(164, 162, 160,
                                                maxColorValue = 255),
                       color   = grDevices::rgb(83,  160, 233,
                                                maxColorValue = 255),
                       na.rm   = TRUE,
                       alpha   = 0.71) +
    ggplot2::geom_point(data    = lichess_move_times,
                        mapping = ggplot2::aes(x = .data$black_ply,
                                               y = -.data$black_times - y_offset),
                        color   = grDevices::rgb(83,  160, 233,
                                                 maxColorValue = 255),
                        size    = 1,
                        na.rm   = TRUE) +
    ggplot2::xlim(c(1, n_ply)) +
    ggplot2::ylim(c(-y_lim, y_lim)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = 'none')
}
