#' Check if a pawn has any legal moves
#'
#' This function checks if a pawn at a given position on a chess board can move
#' forward one square, capture an enemy piece diagonally, or perform an en
#' passant capture.
#'
#' @param board A list containing the current state of the chess board.
#' @param coordinates A vector of length 2 representing the coordinates of the
#'   pawn on the board. The first element is the row number and the second
#'   element is the column number.
#' @param color An optional character vector giving the color of the pawn.
#'   Defaults to the color of the player whose turn it is to move.
#'
#' @return A logical value indicating whether the pawn can move forward one
#'   square, capture an enemy piece diagonally, or perform an en passant capture
#'   (TRUE) or not (FALSE).
#' @examples
#' board <- fen_to_board() # Starting position
#' coordinates <- c(2, 1)  # Coordinates of the white pawn in the starting position
#' can_pawn_move(board, coordinates)
#' @export
can_pawn_move <- function(board, coordinates, color = board$to_move) {
  # Validate input
  assertthat::assert_that(class(board) == "list")
  assertthat::assert_that(all(dim(board$board) == c(8L, 8L)))

  # Set color and move direction
  if (color == "w" || color == "white") {
    color <- "white"
    move <- c(1, 0)  # White pawns move up the board
    captures <- list(c(1, -1), c(1, 1))  # White pawns capture diagonally
    enemy_pieces <- c("p", "n", "b", "r", "q", "k")
  } else if (color == "b" || color == "black") {
    color <- "black"
    move <- c(-1, 0)  # Black pawns move down the board
    captures <- list(c(-1, -1), c(-1, 1))  # Black pawns capture diagonally
    enemy_pieces <- c("P", "N", "B", "R", "Q", "K")
  }
  assertthat::assert_that(color %in% c("white", "black"))

  # Calculate new coordinates for move
  new_coordinates <- coordinates + move

  # Are the new coordinates within the board and is the square empty?
  if (new_coordinates[1] >= 1 && new_coordinates[1] <= 8 &&
      new_coordinates[2] >= 1 && new_coordinates[2] <= 8 &&
      board$board[new_coordinates[1], new_coordinates[2]] == "") {
    if(!move_exposes_king(board, coordinates, new_coordinates, color)) {
      return(TRUE)
    }
  }

  # Loop through each capture
  for (capture in captures) {
    new_coordinates <- coordinates + capture

    # Are the new coordinates within the board and does the square contain an enemy piece?
    if (new_coordinates[1] >= 1 && new_coordinates[1] <= 8 &&
        new_coordinates[2] >= 1 && new_coordinates[2] <= 8 &&
        board$board[new_coordinates[1], new_coordinates[2]] %in% enemy_pieces) {
      if(!move_exposes_king(board, coordinates, new_coordinates, color)) {
        return(TRUE)
      }
    }
  }

  # Check for en passant capture
  if (!is.null(board$ep_target) && board$ep_target != "-") {
    ep_coordinates <- algebraic_to_coordinates(board$ep_target)
    if (abs(coordinates[1] - ep_coordinates[1]) == 1 &&
        abs(coordinates[2] - ep_coordinates[2]) == 1) {
      if(!move_exposes_king(board, coordinates, new_coordinates, color)) {
        return(TRUE)
      }
    }
  }

  # If no legal moves were found, return FALSE
  return(FALSE)
}
