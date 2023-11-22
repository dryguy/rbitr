#' Check if the king can move
#'
#' This function checks if the king has any legal moves.
#'
#' @details See [rbitr::fen_to_board()] for details of the chess board format.
#'   This function is used by other functions when testing for checkmate or
#'   stalemate. `can_king_move()` is an alias for `king_can_move()`.
#'
#' @param board A list containing the current state of the chess board.
#' @param color An optional character vector giving the color of the king.
#'   Defaults to the color of the player whose turn it is to move.
#'
#' @return A logical value indicating whether the king can move. Returns TRUE if
#'   the king has at least one valid move, and FALSE otherwise.
#'
#' @examples
#' board <- fen_to_board() # Starting position
#' king_can_move(board)
#'
#' @export
king_can_move <- function(board, color = board$to_move) {
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

  # Define the piece representation for the king
  king <- ifelse(color == "black", "k", "K")

  # Find the current position of the king
  king_pos <- which(board$board == king, arr.ind = TRUE)

  # Define all possible moves for the king
  moves <- expand.grid(king_pos[1] + -1:1, king_pos[2] + -1:1)

  # Remove the current position of the king from the possible moves
  moves <- moves[!apply(moves == king_pos, 1, all), ]

  # Remove moves that are off the board
  moves <- moves[moves[,1] %in% 1:8 & moves[,2] %in% 1:8, ]

  # Define the friendly pieces
  if(board$to_move == "b") {
    friendly_pieces <- c("p", "n", "b", "r", "q")
  } else {
    friendly_pieces <- c("P", "N", "B", "R", "Q")
  }

  # Delete moves to squares occupied by friendly pieces
  # Apply a function to each row of the moves data frame
  moves <- moves[!apply(moves, 1, function(move) {
    # Convert the move to a coordinate
    coord <- c(move["Var1"], move["Var2"])

    # Check if the cell in the board at the coordinate contains a friendly piece
    piece <- board$board[coord[1], coord[2]]
    return(piece %in% friendly_pieces)
  }), ]

  # If there are no moves left, return FALSE
  if(nrow(moves) == 0) {
    return(FALSE)
  }

  # See if any of the remaining moves don't put the king in check. After
  # updating the board with a move use is_check(updated_board$board, color) to
  # check if the king is in check. If any move is found that doesn't put the
  # king in check, immediately return TRUE.
  enemy_king <- ifelse(color == "white", "k", "K")
  for(i in 1:nrow(moves)) {
    # Get the move
    move <- moves[1, ]

    # Get the coordinates of the move
    coord <- c(move[["Var1"]], move[["Var2"]])

    # Update the board with the move by first replacing the destination square
    # with the king, then replacing the king's original square with an empty
    # square.
    updated_board <- board
    updated_board$board[coord[1], coord[2]] <- king
    updated_board$board[king_pos[1], king_pos[2]] <- ""

    # If the move puts the king next to the enemy king it is not legal
    # Check all neighboring squares for an enemy king
    i <- coord[1]
    j <- coord[2]
    if(any(board$board[max(1, i-1):min(nrow(board$board), i+1), max(1, j-1):min(ncol(board$board), j+1)] == enemy_king)) {
      # Remove the move from the list of possible moves
      moves <- moves[-1, ]
      next
    }

    # See if the king is in check
    if(!is_check(updated_board$board, color)) {
      return(TRUE)
    } else {
      # If the king is in check, remove the move from the list of possible moves
      moves <- moves[-1, ]
    }
  }

  # If any rows are left in moves, return TRUE, else FALSE
  return(nrow(moves) > 0)
}

#' @rdname king_can_move
#' @export
can_king_move <- function(board) {
  king_can_move(board)
}
