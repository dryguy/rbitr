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
  assertthat::assert_that(is.integer(dataframe[, x_name]) |
                            is.numeric(dataframe[, x_name]))
  assertthat::assert_that(is.integer(dataframe[, y_name]) |
                            is.numeric(dataframe[, y_name]))
  assertthat::assert_that(ncol(dataframe) == 2)

  # Make new rows to hold the x-intercept coordinates
  n_row <- nrow(dataframe)
  crossings <- which(abs(diff(sign(dataframe[, y_name]))) == 2) + 1
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

  # Update the dataframe and return
  new_d <- data.frame(x = new_x, y = new_y)
  new_d <- new_d[order(new_d$x), ]
  names(new_d) <- c(x_name, y_name)
  new_d
}
