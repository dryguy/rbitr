#' Check for a stalemate condition in a chess game
#'
#' This function checks if the current player is in a stalemate condition,
#' which means the player is not in check but has no valid moves.
#'
#' @param board A list containing the game state. The list should have a
#'   component named "board" which is an 8x8 matrix representing the game board,
#'   and a component named "to_move" which is a string representing the color
#'   of the player to move ("white" or "black").
#' @param color The color of the player to check for stalemate. If not provided,
#'   the color of the player to move is used.
#'
#' @return TRUE if the player is in stalemate, FALSE otherwise.
#'
#' @export
#'
#' @examples
#' board <- fen_to_board() # Starting position
#' is_stalemate(board)
#' fen <- "8/8/8/2p2p1p/2P2P1k/4pP1P/4P1KP/5BNR w - - 0 19" # Both players in stalemate
#' board <- fen_to_board(fen)
#' is_stalemate(board)
#' is_stalemate(board, "black")
is_stalemate <- function(board, color = board$to_move) {
  # Set color
  if (!color %in% c("black", "white")) {
    color <- ifelse(board$to_move == "b", "black", "white")
  }

  # If the king is in check, it's not a stalemate
  if (is_check(board$board, color)) {
    return(FALSE)
  }

  # Check if the king can move. If it can, it's not a stalemate
  king_coords <- which(board$board == ifelse(color == "white", "K", "k"), arr.ind = TRUE)
  if (can_king_move(board)) {
    return(FALSE)
  }

  # Check if any other piece can move. If they can, it's not a stalemate
  pieces <- c("Q" = "queen", "R" = "rook", "B" = "bishop", "N" = "knight", "P" = "pawn") # Uppercase letters for white pieces
  if (color == "black") {
    names(pieces) <- tolower(names(pieces)) # Lowercase letters for black pieces
  }

  for (piece in names(pieces)) {
    piece_coords <- which(board$board == piece, arr.ind = TRUE)
    move_func <- get(paste0("can_", pieces[piece], "_move"))
    for (coords in split(piece_coords, row(piece_coords))) {
      if (move_func(board, coords, color)) {
        return(FALSE)
      }
    }
  }

  # If none of the above conditions are met, it's a stalemate
  return(TRUE)
}
