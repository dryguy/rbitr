#' Apply `input_function` to the root level elements of a nested list.
#'
#' @param x A nested list.
#' @param input_function A function that takes the root level elements of x as
#     its arguments.
#' @param ... Further arguments to `input_function`.
#'
#' @return A list with the same structure as x, where the root level elements
#'   have been replaced with the outputs of `input_function`.
nested_lapply <- function(x, input_function, ...) {
  lapply_input_function <- function(x, ...) {
    lapply(x, input_function, ...)
  }
  lapply(x, lapply_input_function, ...)
}

# Axis scaling
scale_move_times <- function(move_times) {
  # Use the same logarithmic scale as lichess
  # https://github.com/ornicar/lila/blob/442da0c86a9d54c3cff5645e14d67dfe269a9d0b/public/javascripts/chart/movetime.js
  # logC = Math.pow(Math.log(3), 2)
  # y = Math.pow(Math.log(.005 * Math.min(time, 12e4) + 3), 2) - logC
  move_times[move_times > 12e4] <- 12e4
  log(0.005 * move_times + 3)^2 - log(3)^2
  # move_times / max(move_times, na.rm = TRUE)
}
inv_scale_move_times <- function(scaled_times) {
  200 * (exp(1)^(sqrt(scaled_times + (log(3))^2)) - 3)
}
move_time_trans <- function() scales::trans_new(
  "move_time", scale_move_times, scale_move_times)
winning_chances <- function(scores) {
  # Use the same exponential scale as lichess
  # https://github.com/ornicar/lila/blob/442da0c86a9d54c3cff5645e14d67dfe269a9d0b/public/javascripts/chart/acpl.js
  # y: 2 / (1 + Math.exp(-0.004 * cp)) - 1
  2 / (1 + exp(-0.004 * scores)) - 1
}
inv_winning_chances <- function(scaled_scores) {
  250 * log((-scaled_scores - 1)/(scaled_scores - 1))
}
winning_chances_trans <- function() scales::trans_new(
  "winning_chances", winning_chances, inv_winning_chances)


#' Add x-intercepts
#'
#' Given a data frame of x-y coordinates, find where the x-intercepts would be.
#'
#' @details The input data frame is expected to have two columns, with each row
#'   being the Cartesian coordinates of a point in the x-y plane. If the points
#'   are arranged in order of increasing x values and connected by line
#'   segments, `add_x_intercepts` will find the points where the segments
#'   intersect the x-axis and add those points to the data frame.
#'
#' @param dataframe A 2-column data frame with numeric or integer columns, where
#'   each row is a point in the x-y plane.
#' @param x_name A single-element character vector with the name of the column
#'   containing the x-coordinates.
#' @param y_name A single-element character vector with the name of the column
#'   containing the y-coordinates.
#'
#' @return A new data frame consisting of the original data frame with new rows
#'   added for each intersection with the x-axis.
add_x_intercepts <- function(dataframe, x_name, y_name) {
  # Validate input
  assertthat::assert_that(is.data.frame(dataframe))
  assertthat::assert_that(assertthat::is.string(x_name))
  assertthat::assert_that(assertthat::is.string(y_name))
  assertthat::assert_that(x_name %in% names(dataframe))
  assertthat::assert_that(y_name %in% names(dataframe))
  assertthat::assert_that(is.numeric(dataframe[, x_name]))
  assertthat::assert_that(is.numeric(dataframe[, y_name]))
  assertthat::assert_that(ncol(dataframe) == 2)

  # Make new rows to hold the x-intercept coordinates
  n_row <- nrow(dataframe)
  crossings <- which(abs(diff(sign(dataframe[, y_name]))) == 2) + 1
  if (identical(crossings, numeric(0))) {
    return(dataframe)
  }
  new_y <- 1:(n_row + length(crossings))
  new_y[crossings] <- 0
  new_y[-crossings] <- dataframe[, y_name]
  new_x <- 1:(n_row + length(crossings))
  new_x[-crossings] <- dataframe[, x_name]

  # Calculate the x-intercept coordinates
  x1 <- dataframe[crossings - 1, x_name]
  x2 <- dataframe[crossings, x_name]
  y1 <- dataframe[crossings - 1, y_name]
  y2 <- dataframe[crossings, y_name]
  new_x[crossings] <- (x2 * y1 - x1 * y2) / (y1 - y2)

  # Update the data frame and return
  new_d <- data.frame(x = new_x, y = new_y)
  new_d <- new_d[order(new_d$x), ]
  names(new_d) <- c(x_name, y_name)
  new_d
}


#' Two color area plot
#'
#' @details The input data frame is expected to have two columns, with each row
#'   being the Cartesian coordinates of a point in the x-y plane. The function
#'   `plot_2_color_area` will create an area plot where the area above the
#'   x-axis is filled with white (actually light grey), and the area below the
#'   x-axis is filled with black (actually dark grey).
#'
#' @param dataframe A 2-column data frame with numeric or integer columns, where
#'   each row is a point in the x-y plane.
#' @param x_name A single-element character vector with the name of the column
#'   containing the x-coordinates.
#' @param y_name A single-element character vector with the name of the column
#'   containing the y-coordinates.
#'
#' @return A two-color area plot.
plot_2_color_area <- function(dataframe, x_name, y_name) {
  # Validate input
  assertthat::assert_that(is.data.frame(dataframe))
  assertthat::assert_that(ncol(dataframe) == 2)
  assertthat::assert_that(is.integer(dataframe[, x_name]) |
                            is.numeric(dataframe[, x_name]))
  assertthat::assert_that(is.integer(dataframe[, y_name]) |
                            is.numeric(dataframe[, y_name]))

  # Find the x-intercepts
  dataframe <- add_x_intercepts(dataframe, x_name, y_name)

  # Load background gradient
  gradient_path <- file.path(
    system.file(package = 'rbitr'),
    'extdata',
    'gradient.png'
  )
  gradient_background <- png::readPNG(gradient_path)

  # Generate and return a 2-color area plot
  x_index <- which(colnames(dataframe) %in% x_name)
  y_index <- which(colnames(dataframe) %in% y_name)
  names(dataframe)[x_index] <- 'x'
  names(dataframe)[y_index] <- 'y'
  p1 <- ggplot2::ggplot() +
    ggplot2::annotation_custom(grid::rasterGrob(gradient_background,
                                                width  = grid::unit(1, 'npc'),
                                                height = grid::unit(1, 'npc'))) +
    ggplot2::geom_area(data    = dataframe[dataframe$y <= 0, ],
                       mapping = ggplot2::aes(x = .data$x, y = .data$y),
                       fill    = grDevices::rgb(164, 162, 160,
                                                maxColorValue = 255),
                       alpha   = 0.71) +
    ggplot2::geom_area(data    = dataframe[dataframe$y >= 0, ],
                       mapping = ggplot2::aes(x = .data$x, y = .data$y),
                       fill    = grDevices::rgb(252, 251, 250,
                                                maxColorValue = 255),
                       alpha   = 0.71)
  p1
}
