#' Generate a position from movetext
#'
#' This function accepts a string containing chess moves in Standard Algebraic
#' Notation (SAN), and an integer specifying a number of ply. It returns a
#' shorter string of chess moves in Long Algebraic Notation (LAN).
#'
#' @details Positions are represented by the series of chess moves that led to
#'   the position. The `ply` parameter specifies which position should be
#'   represented in LAN.
#'
#'   This function is useful for generating positions to be analyzed by a chess
#'   engine. The initial position is represented by an empty string "".
#'
#'   The function assumes the input to be valid SAN conforming to the PGN
#'   specification. It does not validate the input or check for legal chess
#'   moves.
#'
#' @param movetext A string containing chess moves in SAN.
#' @param ply An integer specifying a number of ply.
#'
#' @return A character vector of chess positions.
#'
#' @examples
#' movetext <- "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6"
#' get_position(movetext, 3)
#'
#' @export
get_position <- function(movetext, ply) {
  # Validate input
  assertthat::assert_that(assertthat::is.string(movetext))
  assertthat::assert_that(assertthat::is.count(ply) | ply == 0)

  # Handle empty string or 0 ply
  if (movetext == '' | ply == 0) {
    return('')
  }

  # Clean movetext & remove numbering
  movetext <- trimws(gsub("\\d+\\.", "", movetext))
  movetext <- rbitr::clean_movetext(movetext)

  # Split movetext into moves & truncate
  moves <- strsplit(movetext, " ")[[1]]
  assertthat::assert_that(ply <= length(moves))
  moves <- moves[1:ply]

  # Covert to LAN and return
  moves <- paste(moves, collapse = " ")
  return(rbitr::convert_to_lan(moves))
}
