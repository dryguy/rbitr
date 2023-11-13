#' Get the result of a chess game from SAN format moves
#'
#' This function takes a character string of SAN format chess moves and returns
#' the result of the game.
#'
#' @details The functions looks for one of the four possible game termination
#'   markers defined by the PGN specification ("0-1", "1-0", "1/2-1/2", or "*")
#'   and returns the marker if found. If no marker is found it returns NA.
#'
#' @note If multiple markers are present, only the first one is returned.
#'
#' @param movetext A character string of SAN format chess moves.
#'
#' @return The termination marker of the game if found, otherwise NA.
#'
#' @export
#'
#' @examples
#' get_result("1.e4 e5 2.Nf3 Nc6 3.Bb5 a6 4.Ba4 Nf6 1-0")
#' get_result("1.e4 c5 2.Nf3 d6 3.d4 cxd4 4.Nxd4 Nf6 1/2-1/2")
get_result <- function(movetext) {
  # Validate input
  assertthat::assert_that(assertthat::is.string(movetext))

  # Define the possible game termination markers
  markers <- c("0-1", "1-0", "1/2-1/2", "\\*")

  # Use regular expressions to find the termination marker in the movetext
  result <- regmatches(movetext,
                       regexpr(paste(markers, collapse = "|"), movetext))

  # If a marker is found, return it. Otherwise, return NA.
  if (length(result) > 0) {
    return(result)
  } else {
    return(NA)
  }
}
