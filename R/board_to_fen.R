#' Convert a chessboard matrix and status list to a FEN string
#'
#' The function `board_to_fen()` creates a Forsyth-Edwards Notation (FEN) string
#' from an 8x8 matrix of a chessboard and a list containing other information to
#' be stored in the FEN string. See
#' [section 16.1 of the PGN specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c16.1)
#' for detailed information about the FEN format. Also see the documentation for
#' [rbitr::fen_to_board()] for details about the board format.
#'
#' @param board An 8x8 matrix representing a chess board. Each character
#' represents a piece (e.g., 'r' for black rook, 'R' for white rook, etc.), and
#' empty squares are represented by empty strings ('').
#' @param status A list containing the active color ('w' or 'b'), castling
#' availability, en passant target square, halfmove clock, and fullmove number.
#' @return A FEN string representing the board.
#' @export
#'
#' @seealso
#' * [rbitr::fen_to_board()] To convert a FEN string to a board matrix.
#' * [rbitr::print_board()] Prints the board with rows in the correct order.
#'
#' @examples
#' board <- fen_to_board()
#' status <- board[2:6]
#' board <- board[[1]]
#' board_to_fen(board, status)
board_to_fen <- function(board, status) {
  # Initialize an empty FEN string
  fen <- ""

  # Loop over each row of the board from the 8th rank to the 1st
  for (i in 8:1) {
    # Initialize a counter for empty squares
    empty_count <- 0

    # Loop over each square in the current row
    for (j in 1:8) {
      # If the square is empty, increment the counter
      if (board[i, j] == "") {
        empty_count <- empty_count + 1
      } else {
        # If the square is not empty and there were any empty squares before it,
        # add the count of empty squares to the FEN string before adding the piece
        if (empty_count > 0) {
          fen <- paste(fen, empty_count, sep = "")
          empty_count <- 0
        }
        fen <- paste(fen, board[i, j], sep = "")
      }
    }

    # If there were any empty squares at the end of the row, add the count to the FEN string
    if (empty_count > 0) {
      fen <- paste(fen, empty_count, sep = "")
    }

    # Add a slash to separate rows, unless this is the last row
    if (i != 1) {
      fen <- paste(fen, "/", sep = "")
    }
  }

  # Add the rest of the FEN data from the status list using indices instead of names
  fen <- paste(fen, status[[1]], sep = " ")
  fen <- paste(fen, status[[2]], sep = " ")
  fen <- paste(fen, status[[3]], sep = " ")
  fen <- paste(fen, status[[4]], sep = " ")
  fen <- paste(fen, status[[5]], sep = " ")

  return(fen)
}
