#' Convert a series of moves in long algebraic notation to a FEN string
#'
#' @param lan A string representing a series of chess moves in
#'   UCI long algebraic notation (e.g., "e2e4 e7e5 g1f3").
#' @return A string representing the final position after the series of moves
#'   in FEN format.
#' @export
#' @examples
#' lan_to_fen("e2e4 e7e5 g1f3")
lan_to_fen <- function(lan) {
  # Split the series of moves into individual moves
  moves <- strsplit(lan, " ")[[1]]

  # Create the initial board position
  board <- fen_to_board()

  # Loop over each move
  for (move in moves) {
    # Update the board with the move
    board <- update_board(move, board)
  }

  # Convert the updated board to a FEN string
  fen <- board_to_fen(board)

  return(fen)
}
