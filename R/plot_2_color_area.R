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

  # Generate and return a 2-color area plot
  names(dataframe[, x_name]) <- 'x'
  names(dataframe[, y_name]) <- 'y'
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_area(data    = dataframe[dataframe[, y_name] <= 0, ],
                       mapping = ggplot2::aes(x = x, y = y),
                       fill    = grDevices::rgb(190, 188, 186,
                                                maxColorValue = 255),
                       alpha   = 0.75) +
    ggplot2::geom_area(data    = dataframe[dataframe[, y_name] >= 0, ],
                       mapping = ggplot2::aes(x = x, y = y),
                       fill    = grDevices::rgb(252, 251, 250,
                                                maxColorValue = 255),
                       alpha   = 0.75)
  p1
}
