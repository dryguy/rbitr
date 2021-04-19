#' Get the last move.
#'
#' Given a chess position represented as a LAN format sequence of moves, return
#' the most recent move.
#'
#' @param current_position A single element character vector containing a legal
#'   sequence of chess moves in long algebraic notation.
#'
#' @return A single-element character vector of the most recently played move in
#'   LAN format.
#' @export
#'
#' @examples
#' movetext <- 'e2e4 g7g5 b1c3 f7f5 d1h5'
#' get_last_move(movetext)
get_last_move <- function(current_position) {
  if (current_position == '') {
    return('')
  } else {
    moves <- unlist(
      strsplit(current_position, split = ' ', fixed = TRUE),
      use.names = FALSE
    )
    n_ply <- length(moves)
    most_recent_move <- moves[n_ply]
    return(most_recent_move)
  }
}
