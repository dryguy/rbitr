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
