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
