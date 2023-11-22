#' Check if the current board state is a checkmate
#'
#' @param board A list containing the 8x8 matrix `board$board` representing the game state and the color `board$to_move` of the player to move next.
#' @param color The color of the player to check for checkmate. Default is `board$to_move`.
#'
#' @return TRUE if the player of the specified color is in checkmate, FALSE otherwise.
#'
#' @examples
#' # Starting position
#' board <- fen_to_board()
#' is_checkmate(board)
#' # Fool's mate
#' board <- fen_to_board("rnbqkbnr/ppppp1pp/8/8/4pP1q/8/PPPPP1PP/RNBQKBNR w KQkq - 0 3")
#' is_checkmate(board)
#'
#' @export
is_checkmate <- function(board, color = board$to_move) {
  # Convert 'b' and 'w' to 'black' and 'white' if necessary
  if (color %in% c('b', 'w')) {
    color <- ifelse(color == 'b', 'black', 'white')
  }

  # If the king is in check and cannot move, it's checkmate
  if (is_check(board$board, color) && !can_king_move(board)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
