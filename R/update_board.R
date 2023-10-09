#' Update a chessboard to show the position after a move is made
#'
#' This function updates a chessboard to show the position after a move is
#' made. It returns a list containing the updated board and the status of the
#' game.
#'
#' @param move A single-element character vector consisting of a legal chess
#' move in UCI long algebraic notation.
#' @param board A list where the first element is a matrix representing the
#' chessboard, and the remaining 5 elements are the same as in a FEN string.
#' @return A list containing the updated board and status. The list names should
#' be board, to_move, castling_rights, ep_target, halfmove_clock,
#' and fullmove_number.
#' @export
#'
#' @examples
#' board <- fen_to_board()
#' move <- 'e2e4'
#' update_board(move, board)
update_board <- function(move, board) {
  # Convert LAN move to coordinates
  coords <- lan_to_coordinates(move)

  # Check for a piece in the target square
  capture <- board[[1]][coords$target[2], coords$target[1]] != ""

  # Store the piece to move
  piece_to_move <- board[[1]][coords$origin[2], coords$origin[1]]

  # Update the board
  board[[1]][coords$origin[2], coords$origin[1]] <- ""
  board[[1]][coords$target[2], coords$target[1]] <- ifelse(is.null(coords$promotion),
                                                           piece_to_move,
                                                           coords$promotion)

  # Check for castling
  if (abs(coords$origin[1] - coords$target[1]) == 2 && grepl("[kK]", piece_to_move)) {
    # King moved two squares, so it's a castling move
    rook_origin <- ifelse(coords$target[1] > coords$origin[1], 8, 1)
    rook_target <- ifelse(coords$target[1] > coords$origin[1], 6, 4)
    board[[1]][coords$origin[2], rook_origin] <- ""
    board[[1]][coords$origin[2], rook_target] <- ifelse(coords$origin[2] == 1, "R", "r")

    # Update castling rights
    board[[3]] <- gsub(ifelse(coords$origin[2] == 1, "KQ", "kq"), "", board[[3]])
  }

  # Check for non-castling king or rook move
  if (grepl("[kK]", move)) {
    # King moved, so remove castling rights for that color
    board[[3]] <- gsub(ifelse(coords$origin[2] == 1, "KQ", "kq"), "", board[[3]])
  } else if (grepl("[rR]", move) & (coords$origin[1] == 1 | coords$origin[1] == 8)) {
    # Rook moved from original position, so remove corresponding castling right
    board[[3]] <- gsub(ifelse(coords$origin[2] == 1 & coords$origin[1] == 1, "Q",
                              ifelse(coords$origin[2] == 1 & coords$origin[1] == 8, "K",
                                     ifelse(coords$origin[2] == 8 & coords$origin[1] == 1, "q", "k"))), "", board[[3]])
  }

  # If all castling rights are lost, replace with "-"
  if (board[[3]] == "") {
    board[[3]] = "-"
  } else {
    # Ensure correct order of castling rights
    castling_rights <- c("K" = grepl("K", board[[3]]),
                         "Q" = grepl("Q", board[[3]]),
                         "k" = grepl("k", board[[3]]),
                         "q" = grepl("q", board[[3]]))
    board[[3]] <- paste(names(castling_rights)[castling_rights], collapse = "")
  }

  # Check for pawn moving two squares from initial position
  if (abs(coords$target[2] - coords$origin[2]) == 2 && grepl("[pP]", piece_to_move)) {
    # Set en passant target square
    board[[4]] <- paste0(letters[coords$target[1]], (coords$target[2] + coords$origin[2]) / 2)
  } else {
    board[[4]] <- "-"
  }

  # Update halfmove clock
  if (grepl("[pP]", piece_to_move) | capture) {
    board[[5]] <- as.integer(0)
  } else {
    board[[5]] <- as.integer(board[[5]]) + as.integer(1)
  }

  # Update fullmove number and to_move
  if (board[[2]] == "w") {
    board[[2]] <- "b"
  } else {
    board[[2]] <- "w"
    board[[6]] <- as.integer(board[[6]]) + as.integer(1)
  }

  return(board)
}
