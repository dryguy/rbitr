lichess_plot <- function(white_move_times, black_move_times, scores) {
  assertthat::assert_that(is.integer(white_move_times) |
                          is.numeric(white_move_times))
  assertthat::assert_that(is.integer(black_move_times) |
                          is.numeric(black_move_times))
  assertthat::assert_that(is.integer(scores) | is.numeric(scores))
# TODO validate # of moves

  white_move_times <- c(0, white_move_times)
  black_move_times <- c(0, black_move_times)
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
  # Set up background color gradient
  sigma <- 0.2 * y_lim
  y_grad <- seq(from = -y_lim, to = y_lim, length.out = 200)
  x_grad <- seq(from =     (n_ply - 1) / 4 + 1,
                to   = 3 * (n_ply - 1) / 4 + 1,
                length.out = 2)
  grad <- expand.grid(x_grad = x_grad, y_grad = y_grad)
  grad$z_grad <- exp(-(grad$y_grad^2) / (2 * sigma^2))
  # ---
  y_grad2 <- seq(from = -max_y, to = 0, length.out = 100)
  grad2 <- expand.grid(x_grad2 = x_grad, y_grad2 = y_grad2)
  grad2$z_grad2 <- exp(-(grad2$y_grad2^2) * (2 * sigma^2))

  # Make the move times plot
  p_time <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = grad,
              mapping = ggplot2::aes(x_grad, y_grad, fill = z_grad),
              alpha = 0.8) +
    ggplot2::scale_fill_gradient2(low = rgb(216, 214, 212, maxColorValue = 255),
                                 mid  = rgb(237, 235, 233, maxColorValue = 255),
                                 high = rgb(216, 214, 212, maxColorValue = 255)) +
    ggplot2::geom_area(data = lichess_move_times,
      mapping = ggplot2::aes(x = white_ply, y = white_times + y_offset),
      color   = rgb(83,  160, 233, maxColorValue = 255),
      fill    = rgb(252, 251, 250, maxColorValue = 255),
      alpha   = 0.75) +
    ggplot2::geom_point(data = lichess_move_times,
      mapping = ggplot2::aes(x = white_ply, y = white_times + y_offset),
      color   = rgb(83,  160, 233, maxColorValue = 255),
      size = 1) +
    ggplot2::geom_area(data = lichess_move_times,
      mapping = ggplot2::aes(x = black_ply, y = -black_times - y_offset),
      fill    = rgb(177, 175, 174, maxColorValue = 255),
      color   = rgb(83,  160, 233, maxColorValue = 255),
      na.rm   = TRUE) +
    ggplot2::geom_point(data = lichess_move_times,
      mapping = ggplot2::aes(x = black_ply, y = -black_times - y_offset),
      color   = rgb(83,  160, 233, maxColorValue = 255),
      size = 1,
      na.rm   = TRUE) +
    ggplot2::xlim(c(1, n_ply)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::coord_trans(x = 'identity', y = 'move_time')
}
