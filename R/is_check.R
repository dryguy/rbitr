#' Look for checks from enemy pieces
#'
#' This function returns TRUE if there are checks from enemy pieces, and FALSE
#' otherwise.
#'
#' @param board An 8x8 matrix representing a chess position. See
#'   [rbitr::fen_to_board()] for details of the board representation.
#' @param color A string ("white" or "black"), indicating which king to examine.
#'
#' @return TRUE if the king is in check from an enemy piece, FALSE otherwise.
#' @export
#'
#' @examples
#' board <- fen_to_board('8/8/8/8/4k3/8/8/8 b - - 0 1')
#' print(board$board)
#' is_check(board$board, 'black')
#' board$board[3, 4] <- 'P'
#' print(board$board)
#' is_check(board$board, 'black')
is_check <- function(board, color) {
  #Validate input
  assertthat::assert_that(identical(dim(board), c(8L, 8L)))
  assertthat::assert_that(color == 'white' | color == 'black')

  # Locate the king
  king  <- if(color == "white") "K" else "k"
  king_location <- which(board == king, arr.ind = TRUE)

  # Check if the king was found
  if (nrow(king_location) != 1) {
    stop("In is_check(): No king found.")
  }

  # Check for attacking enemy pawns
  enemy_pawn <- if(color == "white") "p" else "P"
  pawn_directions <- if(color == "white") {
    list(c(1, -1), c(1, 1))
  } else {
    list(c(-1, -1), c(-1, 1))
  }
  for (direction in pawn_directions) {
    x <- king_location[1] + direction[1]
    y <- king_location[2] + direction[2]
    if (x >= 1 && x <= 8 && y >= 1 && y <= 8 && board[x, y] == enemy_pawn) {
      return(TRUE)
    }
  }

  # Check for attacking enemy knights
  enemy_knight <- if(color == "white") "n" else "N"
  knight_moves <- list(c(-2, -1), c(-2, 1), c(-1, -2), c(-1, 2),
                       c(1, -2),  c(1, 2),  c(2, -1),  c(2, 1))
  for (move in knight_moves) {
    x <- king_location[1] + move[1]
    y <- king_location[2] + move[2]
    if (x >= 1 && x <= 8 && y >= 1 && y <= 8 && board[x, y] == enemy_knight) {
      return(TRUE)
    }
  }

  # Check each orthogonal direction
  enemies_orthogonal <- if(color == "white") c("r", "q") else c("R", "Q")
  orthogonal_directions <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  for (dir in orthogonal_directions) {
    x <- king_location[1] + dir[1]
    y <- king_location[2] + dir[2]
    while (x >= 1 && x <= 8 && y >= 1 && y <= 8 && board[x, y] == "") {
      x <- x + dir[1]
      y <- y + dir[2]
    }
    if (x >= 1 && x <= 8 && y >= 1 && y <= 8 && board[x, y] %in% enemies_orthogonal) {
      return(TRUE)
    }
  }

  # Check each diagonal direction
  enemies_diagonal <- if(color == "white") c("b", "q") else c("B", "Q")
  diagonal_directions <- list(c(-1, -1), c(-1, 1), c(1, -1), c(1, 1))
  for (dir in diagonal_directions) {
    x <- king_location[1] + dir[1]
    y <- king_location[2] + dir[2]
    while (x >= 1 && x <= 8 && y >= 1 && y <= 8 && board[x, y] == "") {
      x <- x + dir[1]
      y <- y + dir[2]
    }
    if (x >= 1 && x <= 8 && y >= 1 && y <= 8 && board[x, y] %in% enemies_diagonal) {
      return(TRUE)
    }
  }

  # If no checks are found in any direction or by any piece type, return FALSE
  return(FALSE)
}
