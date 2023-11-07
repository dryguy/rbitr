#' Check if a string contains chess moves in standard algebraic notation (SAN)
#'
#' The function uses regular expressions to check if the string's tokens match
#' the expected pattern for chess moves in SAN.
#'
#' @details Before making the check, an attempt is made to remove annotations,
#'   game termination markers, or other content that would disrupt the regular
#'   expression pattern. Note that this function does not check if the moves are
#'   legal.
#'
#' @param movetext A character string, possibly containing a series of chess
#'   moves in SAN.
#'
#' @return TRUE if the string contains chess moves in SAN format, FALSE
#'   otherwise.
#'
#' @examples
#' is_san("1. e4! {best by test} e5 2. Nf3 {black resigns in shame} 1-0")
#' is_san("e2e4 e7e5 g1f3")
#'
#' @export
is_san <- function(movetext) {
  # Validate input
  assertthat::assert_that(assertthat::is.string(movetext))

  # Define the regular expression pattern for SAN
  pattern <- "^([a-hKQBNR]?[a-h]?[x]?[a-h][1-8][+#]?|O-O-O|O-O)\\s*(1-0|0-1|1/2-1/2|\\*)?$"

  # Remove annotations, game termination markers, etc.
  movetext <- rbitr::clean_movetext(movetext)
  movetext <- gsub("[1-9][0-9]*\\.", "", movetext)

  # Split the string on spaces
  tokens <- strsplit(movetext, " ")[[1]]
  tokens <- tokens[tokens != ""]

  # Check each token
  result <- sapply(tokens, function(token) grepl(pattern, token))

  # Return TRUE if all moves are valid, FALSE otherwise
  return(all(result))
}
