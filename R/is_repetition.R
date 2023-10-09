#' Check for n-fold repetition in chess
#'
#' This function checks if a position has occurred n times in a list of
#' chessboards. See [rbitr::fen_to_board()] for details of the board format.
#' The repetition of a position is determined as described in
#' [The FIDE Laws of Chess, Section 9.2](https://handbook.fide.com/chapter/E012023).
#' That is, positions are considered to be repeated only if the pieces are on
#' the same squares, the same side has the move, castling rights are the same,
#' and en passant rights are the same.
#'
#' @param board_list A list of chess boards.
#' @param n The number of repetitions to check for.
#'
#' @return TRUE if any position is repeated n times, FALSE otherwise.
#' @export
#'
#' @examples
#' # Repeat positions by moving knights back and forth
#' boards <- list(fen_to_board()) # starting position
#' boards[[2]] <- update_board('g1f3', boards[[1]])
#' boards[[3]] <- update_board('g8f6', boards[[2]])
#' boards[[4]] <- update_board('f3g1', boards[[3]])
#' boards[[5]] <- update_board('f6g8', boards[[4]]) # back to starting position
#' is_repetition(boards, 3)
#' boards[[6]] <- update_board('g1f3', boards[[5]])
#' boards[[7]] <- update_board('g8f6', boards[[6]])
#' boards[[8]] <- update_board('f3g1', boards[[7]])
#' boards[[9]] <- update_board('f6g8', boards[[8]]) # back to starting position
#' is_repetition(boards, 3)
is_repetition <- function(board_list, n) {
  # Convert each board in the list to a FEN string
  fens <- sapply(board_list, board_to_fen)

  # Drop the halfmove and fullmove counts
  drop_counts <- function(string) {sub(" \\w+ \\w+$", "", string)}
  fens <- lapply(fens, drop_counts)

  # Count the number of times each string appears in the list
  counts <- table(unlist(fens))

  # Check if any count is greater than or equal to n
  any(counts >= n)
}
