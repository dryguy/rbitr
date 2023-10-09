#' Convert FEN notation to a chessboard
#'
#' Forsyth-Edwards Notation (FEN) is a compact way to represent the complete
#' state of a chess game as a single line of ASCII text. In addition to showing
#' the position of the pieces on the board, it stores information about whose
#' turn it is, castling rights, en passant availability, the 50 move rule, and
#' the move number. A FEN string contains six fields separated by spaces.
#'
#' The function `fen_to_board()` makes the information in a FEN string more
#' accessible for use in R. The board representation in converted into an 8x8
#' matrix and is stored in a named list along with the other fields. The names
#' are $board, $to_move, $castling_rights, $ep_target, $halfmove_clock, and
#' $fullmove_number. See
#' [section 16.1 of the PGN specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c16.1)
#' for detailed information about each field.
#'
#' @details In FEN, the pieces are represented as letters, with lowercase for
#' black and uppercase for white.
#'
#' p, P = pawn
#' n, N = knight
#' b, B = bishop
#' r, R = rook
#' q, Q = queen
#' k, K = king
#'
#' Empty squares in FEN are represented by digits indicating a horizontal group
#' of that many empty squares in a row, with rows separated by the / character.
#' For example, the FEN string of the starting position is:
#'
#' 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'
#'
#' Valid FEN strings may be supplied to the `fen_to_board` function using the
#' `fen` argument. Currently, the function does not check the validity of the
#' FEN string, so it is up to the user to ensure that only valid FEN strings are
#' used. If no argument is supplied, the starting position is the default.
#'
#' The FEN piece notation is preserved for matrix representation, but empty
#' squares are represented as empty strings rather than digits. Files a-h have
#' matrix row indices 1-8, and ranks 1-8 have matrix column indices 1-8.
#' Squares can be accessed by their coordinates as `board[rank, file]` (i.e.,
#' square b7 is at `board[7, 2]`). They may also be accessed by column names
#' (i.e., `board[7, 'b']`).
#'
#' @note When printing a matrix in R, row 1 appears at the top. This is the
#' opposite convention from algebraic notation when the chessboard is viewed
#' with white at the bottom, where row 1 is the bottom row. Therefore
#' `print(fen_to_board()$board)` shows a board that has been flipped vertically,
#' as if viewed from below. Rotating the board does not fix the appearance,
#' since the columns will still appear to be in reverse order. To print the
#' board in the conventional manner, use the function [rbitr::print_board()].
#'
#' @param fen A valid FEN string. Defaults to starting position if omitted.
#' @return A named list containing data from the FEN string, including an 8x8
#' matrix of the board.
#' @export
#'
#' @seealso
#' * [rbitr::board_to_fen()] To convert a board matrix to a FEN string.
#' * [rbitr::print_board()] Prints the board with rows in the correct order.
#'
#' @examples
#' board <- fen_to_board()$board
#' # Show the piece on e1
#' board[1, 5]
#' # Show the entire board
#' print_board(board)
fen_to_board <- function(
    fen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1') {
  # Validate input
  assertthat::assert_that(assertthat::is.string(fen))

  # Extract the rows
  fen  <- strsplit(fen,    " ")[[1]]
  rows <- strsplit(fen[1], "/")[[1]]

  # Create an empty matrix to store the board
  board <- matrix(nrow = 8, ncol = 8)

  # Fill the board row by row
  for (i in seq_along(rows)) {
    board[i, ] <- fen_row_to_vector(rows[i])
  }

  # Reverse the board so that it's in the correct order; add row & column names
  board <- board[8:1, ]
  colnames(board) <- letters[1:8]
  rownames(board) <- as.character(1:8)

  return(list(board           = board,
              to_move         = fen[2],
              castling_rights = fen[3],
              ep_target       = fen[4],
              halfmove_clock  = as.integer(fen[5]),
              fullmove_number = as.integer(fen[6])))
}

#' Convert row from FEN string into 8x1 vector
#'
#' The function `fen_row_to_vector()` is a helper function for
#' [rbitr::fen_to_board()].
#'
#' @param row_string A string representing chessboard row in FEN format
#'
#' @return A vector representing a chessboard row, with one element per square
fen_row_to_vector <- function(row_string) {
  # Split the row into individual characters
  split_string <- strsplit(row_string, "")[[1]]

  # Covert digits into empty squares
  row_vector <- c()
  for (char in split_string) {
    if (grepl("\\d", char)) {
      row_vector <- c(row_vector, rep("", as.numeric(char)))
    } else {
      row_vector <- c(row_vector, char)
    }
  }

  return(row_vector)
}

