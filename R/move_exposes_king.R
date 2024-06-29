#' Does the move expose the same-color king to check?
#'
#' This function creates a copy of the board and simulates a move. It then
#' checks if the same-color king would be in check after the move.
#'
#' @param board A list containing the current state of the chess board.
#' @param coordinates A vector of length 2 representing the coordinates of the
#'   piece on the board before the move. The first element is the row number and
#'   the second element is the column number.
#' @param new_coordinates A vector of length 2 representing the coordinates of
#'   the piece on the board after the move. The first element is the row number
#'   and the second element is the column number.
#' @param color A character vector giving the color of the piece.
#'
#' @return A Boolean indicating whether the move results in check.
#'
#' @examples
#' board <- fen_to_board()
#' move_exposes_king(board, c(2, 1), c(3, 1), 'white')
#'
#' @export
move_exposes_king <- function(board, coordinates, new_coordinates, color) {
  # Retrieve the piece from the board using the provided coordinates
  piece <- board$board[coordinates[1], coordinates[2]]

  # Create a copy of the board to test the move
  test_board <- board

  # Remove the piece from its current position
  test_board$board[coordinates[1], coordinates[2]] <- ""

  # Place the piece at the new position
  test_board$board[new_coordinates[1], new_coordinates[2]] <- piece

  # Check if the king is in check after the move
  enemy_color <- ifelse(color == "white", -1L, 1L)
  return(rbitr::is_check_cpp(board_to_int(test_board$board), enemy_color))
}
