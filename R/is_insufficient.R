#' Check for draw by insufficient material
#'
#' If mate is impossible due to lack of material, `is_insufficient()` returns
#' `TRUE`. In cases where mate is theoretically possible `is_insufficient()`
#' will return `FALSE` even if mate can't be forced.
#'
#' Only the following material combinations are considered to be insufficient:
#' - King versus king
#' - King and bishop versus king
#' - King and knight versus king
#' - King versus king with any number of bishops on either side, if all bishops
#'   are on the same color squares
#'
#' @param board An 8x8 matrix representing the chess board.
#' @return A Boolean indicating whether there is insufficient material.
#' @export
#'
#' @examples
#' board <- matrix(rep("", 64), nrow = 8)
#' board[c(1, 64)] <- c("K", "k") # King versus king
#' is_insufficient(board)
is_insufficient <- function(board) {
  # Flatten the board to a vector
  pieces <- as.vector(board)

  # Exit early if sufficient material
  # Any pawns, rooks, or queens are sufficient material
  if (any(pieces %in% c("P", "p", "R", "r", "Q", "q"))) {
    return(FALSE)
  }

  # Count the number of each piece
  piece_counts <- table(pieces)
  piece_counts <- piece_counts[names(piece_counts) != ""]

  # Check for insufficient material
  if (all(names(piece_counts) %in% c("K", "k")) && sum(piece_counts) == 2) {
    # K vs K
    return(TRUE)
  } else if (all(names(piece_counts) %in% c("K", "k", "B", "b")) && sum(piece_counts) == 3) {
    # K vs K + B
    return(TRUE)
  } else if (all(names(piece_counts) %in% c("K", "k", "N", "n")) && sum(piece_counts) == 3) {
    # K vs K + N
    return(TRUE)
  } else if (all(names(piece_counts) %in% c("K", "k", "B", "b")) && sum(piece_counts) > 3) {
    # K + nB vs K + mB
    # Check if all bishops are on the same color squares
    bishop_positions <- which(board == 'B', arr.ind = TRUE)
    bishop_positions <- rbind(bishop_positions, which(board == 'b', arr.ind = TRUE))
    bishop_colors <- (rowSums(bishop_positions) %% 2)
    if (length(unique(bishop_colors)) == 1) {
      return(TRUE)
    }
  }

  # If none of the above conditions are met, there is sufficient material
  return(FALSE)
}
