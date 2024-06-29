#' Convert chessboard characters to integers
#'
#' This function takes an 8x8 character matrix of a chessboard and converts
#' letter representations of pieces into integers, with black pieces indicated
#' using a - sign.
#'
#' @details
#' The board is converted to the same format used by [rbitr::is_check_cpp()].
#' 1 = pawn
#' 2 = bishop
#' 3 = knight
#' 4 = rook
#' 5 = queen
#' 6 = king
#'
#' @param board A character matrix representing a chessboard.
#'
#' @return An integer matrix with the same dimensions as the input, where chess
#'   pieces have been converted to integers.
#'
#' @examples
#' board <- fen_to_board()
#' int_board <- board_to_int(board$board)
#'
#' @export
board_to_int <- function(board) {
  # Define the conversion mapping
  piece_mapping <- c('P' = 1, 'B' = 2, 'N' = 3, 'R' = 4, 'Q' = 5, 'K' = 6,
                     'p' = -1, 'b' = -2, 'n' = -3, 'r' = -4, 'q' = -5, 'k' = -6)

  # Convert the chessboard
  int_board <- matrix(ifelse(board == "", 0, piece_mapping[board]), nrow = 8, ncol = 8)

  # Reverse the row numbers
  int_board <- int_board[nrow(int_board):1,]

  return(int_board)
}
