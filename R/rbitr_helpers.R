#' Apply `input_function` to the root elements of a nested list
#'
#' @param x A nested list.
#' @param input_function A function that takes the root level elements of x as
#'   arguments.
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

#' Scale move times
#'
#' Applies the scaling function log(0.005 * move_times + 3)^2 - log(3)^2 to
#'   the `move_times` parameter.
#'
#' @details The function `scale_move_times()` first applies a cap of 12e4, and
#'   then applies the logarithmic scaling transformation
#'   log(0.005 * `move_times` + 3)^2 - log(3)^2 to the `move_times` parameter.
#'   The inverse function `inv_scale_move_times()` reverses the transformation.
#'
#' @details The scaling function is borrowed from
#' [lichess](https://github.com/ornicar/lila/blob/442da0c86a9d54c3cff5645e14d67dfe269a9d0b/public/javascripts/chart/movetime.js)
#'
#' @note The inverse function cannot recover transformed values that were
#'   greater than 12e4 due to the cap applied by the initial scaling.
#'
#' @param move_times A numeric vector of move times.
#' @param scaled_times A numeric vector of scaled move times.
#'
#' @return For `scaled_move_times()`, a numeric vector of scaled move times. For
#'   `inv_scaled_move_times()` a numeric vector of un-scaled move times.
#'
#' @export
#'
scale_move_times <- function(move_times) {
  # logC = Math.pow(Math.log(3), 2)
  # y = Math.pow(Math.log(.005 * Math.min(time, 12e4) + 3), 2) - logC
  move_times[move_times > 12e4] <- 12e4
  log(0.005 * move_times + 3)^2 - log(3)^2
}
#' @rdname scale_move_times
inv_scale_move_times <- function(scaled_times) {
  200 * (exp(1)^(sqrt(scaled_times + (log(3))^2)) - 3)
}

#' Calculate winning chances
#'
#' Applies the scaling function 2 / (1 + exp(-0.004 * `scores`)) - 1 to the
#'   `scores` parameter.
#'
#' @details The function `winning_chances()` applies the exponential scaling
#'   function 2 / (1 + exp(-0.004 * `scores`)) - 1 to the `scores` parameter.
#'   The inverse function `inv_winning_chances()` reverses the transformation.
#'
#' @details The scaling function is borrowed from
#'   [lichess](https://github.com/ornicar/lila/blob/442da0c86a9d54c3cff5645e14d67dfe269a9d0b/public/javascripts/chart/acpl.js)
#'
#' @param scores A numeric vector of chess engine evaluations.
#' @param scaled_scores A numeric vector of scaled chess engine evaluations.
#'
#' @return A numeric vector of winning chances.
winning_chances <- function(scores) {
  # y: 2 / (1 + Math.exp(-0.004 * cp)) - 1
  2 / (1 + exp(-0.004 * scores)) - 1
}
#' @rdname winning_chances
inv_winning_chances <- function(scaled_scores) {
  250 * log((-scaled_scores - 1)/(scaled_scores - 1))
}

#' Add x-intercepts
#'
#' Given a data frame of x-y coordinates, find the x-intercepts of the line
#'   segments that connect neighboring pairs of points, and add the x-intercepts
#'   to the data frame.
#'
#' @details The input data frame is expected to have two columns, with each row
#'   being the Cartesian coordinates of a point in the x-y plane. If the points
#'   are arranged in order of increasing x values and connected by line
#'   segments, `add_x_intercepts` will find the points where the segments
#'   intersect the x-axis and add those points to the data frame.
#'
#' @param dataframe A 2-column data frame with numeric columns, where each row
#'   is a point in the x-y plane.
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


#' Two-color area plot
#'
#' Make an area plot where shading above the x-axis is white, and shading below
#'   the x-axis is dark gray.
#'
#' @details The input data frame is expected to have two columns, with each row
#'   being the Cartesian coordinates of a point in the x-y plane. The function
#'   `plot_2_color_area` will create an area plot where the area above the
#'   x-axis is filled with white (actually light gray), and the area below the
#'   x-axis is filled with black (actually dark gray).
#'
#' @param dataframe A 2-column data frame with numeric or integer columns, where
#'   each row is a point in the x-y plane.
#' @param x_name A single-element character vector with the name of the column
#'   containing the x-coordinates.
#' @param y_name A single-element character vector with the name of the column
#'   containing the y-coordinates.
#' @param background (Default = 'none') A single-element character vector
#'   indicating the desired background. Options are 'none' or 'gradient'.
#'
#' @return A ggplot object that is a two-color area plot.
plot_2_color_area <- function(dataframe, x_name, y_name, background = 'none') {
  # Validate input
  assertthat::assert_that(is.data.frame(dataframe))
  assertthat::assert_that(ncol(dataframe) == 2)
  assertthat::assert_that(is.integer(dataframe[, x_name]) |
                          is.numeric(dataframe[, x_name]))
  assertthat::assert_that(is.integer(dataframe[, y_name]) |
                          is.numeric(dataframe[, y_name]))
  assertthat::assert_that(background == 'none' |
                          background == 'gradient')
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
  if (background == 'gradient') {
    p_background <- ggplot2::annotation_custom(grid::rasterGrob(gradient_background,
                                               width  = grid::unit(1, 'npc'),
                                               height = grid::unit(1, 'npc')))
  } else if (background == 'none') {
    p_background <- ggplot2::geom_blank()
  }
  if (nrow(dataframe[dataframe$y <= 0, ]) > 1) {
    area1 <- ggplot2::geom_area(data    = dataframe[dataframe$y <= 0, ],
                                mapping = ggplot2::aes(x = .data$x, y = .data$y),
                                fill    = grDevices::rgb(164, 162, 160,
                                                         maxColorValue = 255),
                                alpha   = 0.71)
  } else {
    area1 <- ggplot2::geom_blank()
  }
  if (nrow(dataframe[dataframe$y >= 0, ]) > 1) {
    area2 <- ggplot2::geom_area(data    = dataframe[dataframe$y >= 0, ],
                                mapping = ggplot2::aes(x = .data$x, y = .data$y),
                                fill    = grDevices::rgb(252, 251, 250,
                                                         maxColorValue = 255),
                                alpha   = 0.71)
  } else {
    area2 <- ggplot2::geom_blank()
  }
  p1 <- ggplot2::ggplot() +
    p_background + area1 + area2
  p1
}

#' Add spaces
#'
#' Justify table columns by adding spaces.
#'
#' @details The function `add_spaces()` is used to make justified tables with a
#'   monospaced font. The parameter `my_text` should contain a vector of text
#'   for a single column, with each row as a separate element. For right-
#'   justified text, set the `to` parameter to 'head'. This will add enough
#'   space to the front of each row to make all entries have an equal number of
#'   characters. Similarly, for left-justified text, set the `to` parameter to
#'   'tail'.
#'
#' @param my_text A character vector of text to be justified.
#' @param to A single-element character vector with allowed values of either
#'   'head' or 'tail' to indicate on which side spaces should be added.
#'
#' @return A character vector of text with spaces added to give each entry an
#'   equal number of characters.
#' @export
#'
#' @examples
#' add_spaces(c('a', 'b', 'cccc'), to = 'tail')
add_spaces <- function(my_text, to) {
  # Validate input
  assertthat::assert_that(is.vector(my_text))
  assertthat::assert_that(to == 'head' | to == 'tail')
  n_char <- unlist(lapply(my_text, nchar))
  max_char <- max(n_char, na.rm = TRUE)
  n_spaces <- max_char - n_char
  add_to_head <- function(index, my_text, n_spaces) {
    paste0(paste0(rep(' ', n_spaces[[index]]), collapse = ''), my_text[[index]], ' ')
  }
  add_to_tail <- function(index, my_text, n_spaces) {
    paste0(my_text[[index]], paste0(rep(' ', n_spaces[[index]]), collapse = ''), '\n')
  }
  index <- 1:length(my_text)
  if (to == 'head') {
    return(unlist(lapply(index, add_to_head, my_text, n_spaces)))
  }
  if (to == 'tail') {
    return(unlist(lapply(index, add_to_tail, my_text, n_spaces)))
  }
}

#' Make a table
#'
#' Makes a two-column table that will be justified when rendered with a
#'   monospaced font.
#'
#' @details A character string will be generated that produces a two-column
#'   table when rendered in a monospaced font. The first column will be right-
#'   justified, and the second column will be left-justified. Rows will be
#'   separated by newline characters ('\\n').
#'
#' @param col_a A character vector of the rows of column A.
#' @param col_b A character vector of the rows of column B.
#'
#' @return A single-element character vector of text that will be a two-column
#'   table when rendered with a monospaced font.
#' @export
#' @examples
#' cat(make_table(c('1', '2', '11'), c('one', 'two', 'eleven')))
make_table <- function(col_a, col_b) {
  assertthat::assert_that(is.character(col_a))
  assertthat::assert_that(is.character(col_b))
  assertthat::assert_that(length(col_a) == length(col_b))
  col_a <- add_spaces(col_a, 'head')
  col_b <- add_spaces(col_b, 'tail')
  new_table <- matrix(c(col_a, col_b), ncol = 2)
  new_table <- as.vector(t(new_table))
  paste0(new_table, collapse = '')
}
