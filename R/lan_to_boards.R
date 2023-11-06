#' Convert UCI LAN to a list of boards
#'
#' This function takes a string of moves in UCI LAN (Long Algebraic Notation)
#' and converts it into a list of boards. Each board represents the state of the
#' game after each move.
#'
#' @param lan A string of chess moves in UCI LAN.
#' @return A list of boards representing the state of the game after each move.
#' @export
#' @examples
#' lan_to_boards("e2e4 e7e5 g1f3 b8c6 f1b5")
lan_to_boards <- function(lan) {
  # Split the LAN string into individual moves
  moves <- unlist(strsplit(lan, " ", fixed = TRUE))

  # Initialize a list to store the boards
  boards <- vector('list', length(moves) + 1)

  # The first board is the initial state of the game
  boards[[1]] <- fen_to_board()

  i <- 1

  # Loop over each move
  for (move in moves) {
    i <- i + 1

    # Update the board with the current move
    boards[[i]] <- update_board(move, boards[[i - 1]])
  }

  return(boards)
}

