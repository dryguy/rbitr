#' Convert a chessboard state to a FEN string
#'
#' The purpose of `board_to_fen()` is simply to reverse the operation of
#' [rbitr::fen_to_board()]. As such, `board_to_fen()` accepts as input a list
#' of game data (`board`) in the format produced by `fen_to_board()`.
#'
#' The`board` argument is a named list containing the same data as a FEN string,
#' but in a format that is more accessible in R:
#'
#' `$board` An 8x8 character matrix representing the chessboard, with lowercase
#' letters for the black pieces and uppercase for white.
#' `$to_move`: Whose turn it is. Either 'w' or 'b'.
#' `$castling_rights`: Which castling privileges remain. Can be any combination
#' of the characters 'K', 'Q', 'k', 'q',  in order from uppercase to lowercase
#' and then king before queen to show queenside and kingside castling rights for
#' white or black, or '-' if no castling rights remain.
#' `$ep_target`: If the previous move was a two-square pawn advance,
#' `$ep_target` will give the name of the square where an enemy pawn capturing
#' *en passant* would land, even if no enemy pawn can currently capture
#' *en passant*. Otherwise, the value is '-'.
#' `$halfmove_clock`: An integer for keeping track of the number of half moves
#' since the last pawn move or capture. Used to detect 50 move rule draws.
#' `$fullmove_number`: An integer showing the current move number. Increases by
#' one after every black move.
#'
#' See
#' [section 16.1 of the PGN specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c16.1)
#' for detailed information about the FEN format. Also see the documentation for
#' [rbitr::fen_to_board()] for more details about the board format.
#'
#' @param board A list containing an 8x8 matrix of the board, whose trun it is,
#' castling rights, en passant target square, halfmove clock, and fullmove
#' number.
#' @return A FEN string representing the board.
#' @export
#'
#' @seealso
#' * [rbitr::fen_to_board()] To convert a FEN string to a board matrix.
#' * [rbitr::print_board()] Prints the board with rows in the correct order.
#'
#' @examples
#' board <- fen_to_board()
#' board_to_fen(board)
board_to_fen <- function(board) {
  # Validate input
  assertthat::assert_that(is.list(board))
  assertthat::assert_that(length(board) == 6)
  assertthat::assert_that(identical(dim(board[[1]]), c(8L, 8L)))
  assertthat::assert_that(is.character(board[[2]]))
  assertthat::assert_that(is.character(board[[3]]))
  assertthat::assert_that(is.character(board[[4]]))
  assertthat::assert_that(is.integer(board[[5]]))
  assertthat::assert_that(is.integer(board[[6]]))

  # Extract data
  status <- board[2:6]
  board <- board[[1]]

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

    # Add empty squares at the end of the row
    if (empty_count > 0) {
      fen <- paste(fen, empty_count, sep = "")
    }

    # Add a slash to separate rows, unless this is the last row
    if (i != 1) {
      fen <- paste(fen, "/", sep = "")
    }
  }

  # Add the rest of the FEN data from the status list
  fen <- paste(fen, status[[1]], sep = " ")
  fen <- paste(fen, status[[2]], sep = " ")
  fen <- paste(fen, status[[3]], sep = " ")
  fen <- paste(fen, status[[4]], sep = " ")
  fen <- paste(fen, status[[5]], sep = " ")

  return(fen)
}
