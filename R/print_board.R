#' Print a chessboard in the conventional orientation
#'
#' Prints a chessboard with the rows arranged bottom to top.
#'
#' @details The conventional orientation of a 2-D chessboard places row 1 at
#' the bottom from white's point of view. R's `print()` function prints matrices
#' with row 1 at the top, causing chessboard rows to appear in reverse order,
#' resulting in a mirror image of the board. The `print_board()` function simply
#' a wrapper for `print()` that modifies the board before printing to give a
#' true image of the board.
#'
#' @param board An 8x8 matrix representing a chessboard. See
#' [rbitr::fen_to_board()] for details of the board representation.
#' @param pov (Default = 'white') A string indicating which side's point of view
#' should be shown. Allowed values are 'black' and 'white'.
#'
#' @return Prints a chessboard in the correct orientation.
#' @export
#'
#' @examples
#' board <- fen_to_board()$board
#' print_board(board)
print_board <- function(board, pov = 'white') {
  # Validate input
  assertthat::assert_that(pov == 'white' | pov == 'black')
  assertthat::assert_that(is.matrix(board))
  assertthat::assert_that(identical(dim(board), c(8L, 8L)))

  # Print the correct orientation
  if(pov == 'white') {
    print(board[nrow(board):1, ])
  } else {
    print(apply(board, 1, rev))
  }
}

