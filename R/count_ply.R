#' Count the moves, half-moves, or positions in a chess game
#'
#' * `count_moves()` counts the number of moves in the game.
#' * `count_halfmoves()` and `count_ply()` count the number of half-moves in the game.
#' * `count_positions()` and `count_nodes()` count the number of positions in the game.
#'
#' @details In chess, the following terms are used to describe the progression
#'   of the game:
#'
#'   Move: A move in chess refers to a sequence in which each player has played
#'   one turn. So, one move consists of white playing a "half-move" followed by
#'   black playing another "half-move." Thus the number of moves can be an
#'   integer or half-integer value. The number of moves is given by the function
#'   `count_moves()`. Repeated moves are included in the count.
#'
#'   Half-Move or Ply: A half-move is a single turn taken by one player. The
#'   number of half-moves is given by the function `count_halfmoves()` or its
#'   alias `count_ply()`. Repeated half-moves are included in the count.
#'
#'   Position or Node: A chess position is the arrangement of the pieces on the
#'   board along with the information about castling rights, *en passant*
#'   captures, the side to move, and the 50 move rule. Half-moves are
#'   transitions from one position to the next, thus the number of positions is
#'   always one more than the number of half-moves. The number of positions in a
#'   game is given by the function `count_positions()` or its alias
#'   `count_nodes()`. The starting position is always included in the count.
#'   Repeated positions are also included in the count.
#'
#' @param movetext A character string containing the movetext of a chess game in
#'   either standard algebraic notation (SAN) or
#'   [UCI-format](http://wbec-ridderkerk.nl/html/UCIProtocol.html) long
#'   algebraic notation (LAN).
#'
#' @return
#' * `count_ply()` and `count_halfmoves()` return an integer of the number of half-moves in the game.
#' * `count_nodes()` and `count_positions()` return an integer of the number of positions in the game.
#' * `count_moves()` returns the number of moves in the game, which can take on integer or half-integer values.
#'
#' @export
#'
#' @rdname count_functions
#'
#' @examples
#' count_moves("1. e4 e5 2. Nf3 Nc6")
#' count_ply("1. e4 e5 2. Nf3 Nc6")
#' count_halfmoves("1. e4 e5 2. Nf3 Nc6")
#' count_positions("1. e4 e5 2. Nf3 Nc6")
#' count_nodes("1. e4 e5 2. Nf3 Nc6")
count_ply <- function(movetext) {
  return(count_halfmoves(movetext))
}

#' @rdname count_functions
#' @export
count_nodes <- function(movetext) {
  return(count_ply(movetext) + 1L)
}

#' @rdname count_functions
#' @export
count_positions <- function(movetext) {
  return(count_ply(movetext) + 1L)
}

#' @rdname count_functions
#' @export
count_moves <- function(movetext) {
  return(count_halfmoves(movetext) / 2)
}

#' @rdname count_functions
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
