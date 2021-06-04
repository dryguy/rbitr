#' Time plot
#'
#' Generate a plot of scaled move times.
#'
#' @details Move times (in seconds) are plotted using a scaling function
#'   borrowed from [lichess.org](lichess.org). The
#'   [logarithmic scaling function](https://github.com/ornicar/lila/blob/442da0c86a9d54c3cff5645e14d67dfe269a9d0b/public/javascripts/chart/movetime.js)
#'   keeps short move times from disappearing into the baseline. White move
#'   times are shown as positive values, with a white fill, while black move
#'   times are shown as negative values with a dark gray fill.
#'
#' @param white_move_times A numeric vector of move times for white, in seconds.
#' @param black_move_times A numeric vector of move times for black, in seconds.
#' @param style (Default = 'graph') A single-element character vector
#'   indicating the plot style. Allowed values are 'graph' for a traditional
#'   graph with axes, or 'infographic' to add a background gradient and remove
#'   the axes (similar to [lichess.org](lichess.org)).
#'
#' @return A ggplot object of the plotted data.
#' @export
#'
#' @seealso
#'   * [rbitr::advantage_plot()] to plot advantage data.
#'   * [rbitr::lichess_plot()] to plot move time and advantage data with a table
#'     of game stats.
#'
#' @examples
#'   white_move_times <- c(4, 10, 5, 10)
#'   black_move_times <- c(3, 4, 7)
#'   scaled_time_plot(white_move_times, black_move_times)
scaled_time_plot <- function(white_move_times, black_move_times, style = 'graph') {
  # Validate input
  assertthat::assert_that(is.numeric(white_move_times))
  assertthat::assert_that(is.numeric(black_move_times))
  assertthat::assert_that(length(white_move_times) -
                          length(black_move_times) < 2)
  assertthat::assert_that(style == 'graph' |
                          style == 'infographic')
  n_white_move_times <- length(white_move_times)
  # Create a data frame of move times
  white_move_times <- c(0, scale_move_times(white_move_times))
  black_move_times <- c(0, scale_move_times(black_move_times))
  max_y <- max(c(white_move_times, black_move_times))
  if (n_white_move_times == 0) {
    max_y == 1
  }
  y_offset <- 0.01 * max_y
  y_lim <- 1.1 * max_y
  if (length(black_move_times) < length(white_move_times)) {
    black_move_times <- c(black_move_times, NA)
  }
  n_ply <- length(white_move_times) + length(black_move_times)
  ply <- 1:n_ply
  lichess_move_times <- data.frame(
    white_ply = ply[ply %% 2 == 1],
    black_ply = ply[ply %% 2 == 0],
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
  if (style == 'infographic') {
    p_background <- ggplot2::annotation_custom(
      grid::rasterGrob(gradient_background,
                       width  = grid::unit(1, 'npc'),
                       height = grid::unit(1, 'npc')))
    p_theme <- ggplot2::theme_void()
  } else if (style == 'graph') {
    p_background <- ggplot2::geom_blank()
    p_theme <- ggplot2::geom_blank()
  }

  # Make the move time plot
  p_time <- ggplot2::ggplot() +
    p_background +
    ggplot2::geom_area(data    = lichess_move_times,
                       mapping = ggplot2::aes(x = .data$white_ply,
                                              y = .data$white_times + y_offset),
                       color   = grDevices::rgb(83,  160, 233,
                                                maxColorValue = 255),
                       fill    = grDevices::rgb(252, 251, 250,
                                                maxColorValue = 255),
                       alpha   = 0.71) +
    ggplot2::geom_area(data    = lichess_move_times,
                       mapping = ggplot2::aes(x = .data$black_ply,
                                             y = -.data$black_times - y_offset),
                       fill    = grDevices::rgb(164, 162, 160,
                                                maxColorValue = 255),
                       color   = grDevices::rgb(83,  160, 233,
                                                maxColorValue = 255),
                       na.rm   = TRUE,
                       alpha   = 0.71) +
    ggplot2::ylim(c(-y_lim, y_lim)) +
    ggplot2::xlab('Half Moves') +
    ggplot2::ylab('Move Times (Scaled)') +
    ggplot2::scale_x_continuous(
      breaks = function(x) unique(floor(pretty(x, 20))),
      limits = c(1, n_ply - 1)) +
    ggplot2::theme(legend.position = 'none') +
    p_theme

  if (n_white_move_times == 0) {
    return(suppressMessages(
      p_time +
        ggplot2::xlim(c(-1, 1)) +
        ggplot2::ylim(c(-1, 1)) +
        ggplot2::annotate('text', x = 0, y = 0,
                          label = 'Move Time Data Not Available',
                          color = grDevices::rgb(83,  160, 233,
                                                 maxColorValue = 255))
    ))
  } else {
    return(suppressMessages(
      p_time +
        ggplot2::geom_point(data    = lichess_move_times,
                            mapping = ggplot2::aes(x = .data$white_ply,
                                                   y = .data$white_times +
                                                     y_offset),
                            color   = grDevices::rgb(83,  160, 233,
                                                     maxColorValue = 255),
                            size    = 1) +
        ggplot2::geom_point(data    = lichess_move_times,
                            mapping = ggplot2::aes(x = .data$black_ply,
                                                   y = -.data$black_times -
                                                     y_offset),
                            color   = grDevices::rgb(83,  160, 233,
                                                     maxColorValue = 255),
                            size    = 1,
                            na.rm   = TRUE)
    ))
  }
}
