#' Get points from chess match results
#'
#' @description This function takes a vector of chess match results as input and
#' converts them to tournament points.
#'
#' @details Each result should be one of the following strings: "0-1", "1-0",
#' "1/2-1/2", or "*". The function returns 0, 1, 0.5, or NA respectively for
#' each result. If a result is not one of these four strings, it throws an
#' error.
#'
#' @param results A vector of strings representing the results of chess matches.
#' @return A numeric vector representing the points from the chess matches.
#' @examples
#' get_points(c("0-1", "1-0", "1/2-1/2", "*")) # returns c(0, 1, 0.5, NA)
#' @export
get_points <- function(results) {
  unname(sapply(results, function(result) {
    if (result == "0-1") {
      return(0)
    } else if (result == "1-0") {
      return(1)
    } else if (result == "1/2-1/2") {
      return(0.5)
    } else if (result == "*") {
      return(NA)
    } else {
      stop(glue::glue("Invalid argument to get_points: {result}. Expected one of '0-1', '1-0', '1/2-1/2', '*'"))
    }
  }))
}
