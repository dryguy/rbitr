#' Check if a string contains chess moves in UCI-format long algebraic notation
#' (LAN)
#'
#' The function uses regular expressions to check if the string's tokens match
#' the expected pattern for chess moves in LAN.
#'
#' @details UCI-format long algebraic notation is described in the [UCI
#'   protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html). Note that this
#'   function does not check if the moves are legal.
#'
#' @param movetext A character string, possibly containing a series of chess
#'   moves in SAN.
#'
#' @return TRUE if the string contains valid chess moves in LAN, FALSE otherwise
#'
#' @examples
#' is_ucilan("e2e4 e7e5 g1f3 b8c6")
#' is_ucilan("1. e4 e5 2. Nf3 Nc6")
#'
#' @export
is_ucilan <- function(movetext) {
  # Validate input
  assertthat::assert_that(assertthat::is.string(movetext))

  # Define the regular expression pattern for LAN
  pattern <- "^[a-h][1-8][a-h][1-8][qrbnQRBN]?$"

  # Split the movetext into individual moves
  moves <- strsplit(movetext, " ")[[1]]

  # Check each move
  result <- sapply(moves, function(move) grepl(pattern, move))

  # Return TRUE if all moves are valid, FALSE otherwise
  return(all(result))
}
