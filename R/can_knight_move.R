#' Check if a knight has any legal moves
#'
#' This function checks if a knight at a given position on a chess board has any
#' legal moves.
#'
#' @details See [rbitr::fen_to_board()] for details of the chess board format.
#'   This function is used by other functions when testing for stalemate.
#'   `can_knight_move()` is an alias for `knight_can_move()`.
#'
#' @param board A list containing the current state of the chess board.
#' @param coordinates A vector of length 2 representing the coordinates of the
#'   knight on the board. The first element is the row number and the second
#'   element is the column number.
#' @param color An optional character vector giving the color of the knight.
#'   Defaults to the color of the player whose turn it is to move.
#'
#' @return A logical value indicating whether the knight has any legal moves
#'   (TRUE) or not (FALSE).
#' @examples
#' board <- fen_to_board() # Starting position
#' coordinates <- c(1, 2)  # Coordinates of the white knight in the starting position
#' can_knight_move(board, coordinates)
#' @export
can_knight_move <- function(board, coordinates, color = board$to_move) {
  # Validate input
  assertthat::assert_that(class(board) == "list")
  assertthat::assert_that(all(dim(board$board) == c(8L, 8L)))

  # Set color
  if (color == "w") {
    color <- "white"
  } else if (color == "b") {
    color <- "black"
  }
  assertthat::assert_that(color %in% c("white", "black"))

  # Define the moves a knight can make
  moves <- list(c(-2, -1), c(-2, 1), c(-1, -2), c(-1, 2),
                c(1, -2), c(1, 2), c(2, -1), c(2, 1))

  # Loop through each move
  for (move in moves) {
    new_coordinates <- coordinates + move

    # Are the new coordinates within the board?
    within_board <- new_coordinates[1] >= 1 &&
      new_coordinates[1] <= 8 &&
      new_coordinates[2] >= 1 &&
      new_coordinates[2] <= 8
    if (!within_board) {
      next
    }

    # Is the square occupied by a friendly piece?
    if(color == "white") {
      has_friendly <- board$board[new_coordinates[1], new_coordinates[2]] %in%
        c("P", "N", "B", "R", "Q", "K")
    } else {
      has_friendly <- board$board[new_coordinates[1], new_coordinates[2]] %in%
        c("p", "n", "b", "r", "q", "k")
    }
    if (has_friendly) {
      next
    }

    # Make a copy of the board and move the knight to the new coordinates
    new_board <- board$board
    new_board[coordinates[1], coordinates[2]] <- ""
    new_board[new_coordinates[1], new_coordinates[2]] <-
      board$board[coordinates[1], coordinates[2]]
    # Does moving to the new coordinates put the king in check?
    is_legal <- !is_check(new_board, color)
    if (!is_legal) {
      next
    }

    # Is the square at the new coordinates empty?
    is_empty <- board$board[new_coordinates[1], new_coordinates[2]] == ""
    if (is_empty) {
      return(TRUE)
    }

    # Does the square at the new coordinates contain an enemy piece?
    if(color == "white") {
      has_enemy <- board$board[new_coordinates[1], new_coordinates[2]] %in% c("p", "n", "b", "r", "q", "k")
    } else {
      has_enemy <- board$board[new_coordinates[1], new_coordinates[2]] %in% c("P", "N", "B", "R", "Q", "K")
    }
    if (has_enemy) {
      return(TRUE)
    }
  }

  # If no legal moves were found in any direction, return FALSE
  return(FALSE)
}

#' @rdname can_knight_move
#' @export
knight_can_move <- function(board, coordinates, color = board$to_move) {
  can_knight_move(board, coordinates, color)
}
