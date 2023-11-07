#' Count the number of ply or halfmoves in a chess game
#'
#' The functions `count_ply()` and `count_halfmoves()` take the movetext of a
#' chess game as input and return either the number of ply (positions) or the
#' number of half-moves in the game.
#'
#' @rdname counts
#'
#' @details Repeated positions or repeated half moves are included in the count.
#'   The number of half-moves is always one less than the number of ply.
#'
#' @param movetext A character string containing the movetext of a chess game in
#'   either standard algebraic notation (SAN) or
#'   [UCI-format](http://wbec-ridderkerk.nl/html/UCIProtocol.html) long
#'   algebraic notation (LAN).
#'
#' @return
#' * `count_ply()` returns an integer of the number of ply in the game.
#' * `count_halfmoves()` returns an integer of the number of half-moves in the game.
#'
#' @export
#'
#' @examples
#' count_ply("1. e4 e5 2. Nf3 Nc6")
#' count_halfmoves("1. e4 e5 2. Nf3 Nc6")
count_ply <- function(movetext) {
  return(count_halfmoves(movetext) + 1L)
}

#' @rdname counts
#' @export
count_halfmoves <- function(movetext) {
  # Validate input
  assertthat::assert_that(assertthat::is.string(movetext))

  # Remove annotations, game termination markers, etc.
  movetext <- clean_movetext(movetext)

  # Convert SAN to LAN
  if (is_san(movetext)) {
    movetext <- convert_to_lan(movetext)
  }

  # Remove move numbers
  movetext <- gsub("\\d+\\.+", "", movetext)

  # Split the movetext into individual moves
  moves <- unlist(strsplit(movetext, " "))

  # Remove empty elements
  moves <- moves[moves != ""]

  # Return the number of moves
  return(length(moves))
}
