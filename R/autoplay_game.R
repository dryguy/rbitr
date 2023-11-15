#' Have a chess engine play against itself
#'
#' The function `autoplay_game()` is used to make a
#' [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#' engine play a game against itself. Since engines typically do not handle
#' draws by repetition, insufficient material, or the fifty-move rule, the
#' function checks for those conditions and declares a draw if any are detected.
#'
#' @param engine_path A single-element character vector of the path to a UCI
#' compatible chess engine.
#' @param position A single-element character vector containing a series of
#' legal chess moves in long algebraic notation (LAN).
#' @param limiter A single-element character vector indicating the desired
#' mode of search termination. Allowed values are 'depth' (to search a fixed
#' number of plies), 'nodes' (to search a fixed number of nodes), and
#' 'movetime' (to search for a fixed number of milliseconds).
#' @param limit A single-element integer vector of the desired search depth
#' (# of plies), search nodes (# of nodes), or search time (# of milliseconds).
#' @param n_cpus (Default = 1) A single-element integer vector of the number of
#' CPUs to use.
#' @param hash_size (Default = NULL) A single-element integer vector of the
#' desired hash size, in MB.
#' @param mute (Default = TRUE) A boolean indicating if the board should be
#' printed after each turn.
#' @param ply_limit (Default = NULL) An integer indicating the maximum number
#' of ply before the game is stopped. Used to limit execution time for unit
#' tests and examples.
#'
#' @return A single-element character vector of the game in standard algebraic
#' notation (SAN).
#' @export
#'
#' @examples
#' # Replace '/stockfish.exe' with a path to your UCI-compatible engine. To play
#' # a full game, delete the ply_limit.
#' autoplay_game('/stockfish.exe', limiter = 'depth', limit = 1, n_cpus = 1,
#' mute = FALSE, ply_limit = 3)
autoplay_game <- function(engine_path, position = '', limiter, limit, n_cpus,
                          hash_size = NULL, mute = TRUE, ply_limit = NULL) {
  # Validate the input
  assertthat::assert_that(assertthat::is.string(position))
  assertthat::assert_that(assertthat::is.string(engine_path))
  assertthat::assert_that(assertthat::is.count(n_cpus))
  assertthat::assert_that(assertthat::is.string(limiter))
  assertthat::assert_that(limiter %in% c('depth', 'nodes', 'movetime'))
  assertthat::assert_that(assertthat::is.count(limit))
  assertthat::assert_that(assertthat::is.count(hash_size) |
                            is.null(hash_size))
  assertthat::assert_that(assertthat::is.flag(mute))
  assertthat::assert_that(is.null(ply_limit) | assertthat::is.count(ply_limit))

  # Convert LAN to boards
  ply <- count_ply(position) + 1
  boards <- list()
  moves <- position
  if (position != '') {
  #  Fill in boards so far
    boards <- lan_to_boards(moves)
  }
  boards[[ply]] <- fen_to_board(lan_to_fen(position))
  if (!mute) {
    print_board(boards[[ply]]$board)
  }

  # Play the game
  positionlog <- rbitr::evaluate_position(position, engine_path, limiter, limit,
                                          n_cpus, n_pv = 1, hash_size)
  bestmove <- sub(".*bestmove (\\w+).*", "\\1", utils::tail(positionlog[[1]], 1))
  ply <- ply + 1L
  boards[[ply]] <- update_board(bestmove, boards[[ply - 1L]])
  if (!mute) {
    print(bestmove)
    print_board(boards[[ply]]$board)
  }
  moves <- trimws(paste0(position, ' ', bestmove))
  while(TRUE) {
    positionlog <- rbitr::evaluate_position(moves, engine_path, limiter,
                                            limit, n_cpus, n_pv = 1, hash_size)
    bestmove <- sub(".*bestmove (\\w+).*", "\\1", utils::tail(positionlog[[1]], 1))
    if (bestmove == 'bestmove (none)') {
      break
    }
    if (!is.null(ply_limit)) {
      if (ply >= ply_limit) {
        moves <- paste0(moves, ' ', bestmove)
        moves <- bigchess::lan2san(moves)
        return(paste0(moves, ' * {reached ply limit}'))
      }
    }
    ply <- ply + 1L
    boards[[ply]] <- update_board(bestmove, boards[[ply - 1L]])
    if (boards[[ply]]$halfmove_clock >= 100) {
      moves <- paste0(moves, ' ', bestmove)
      moves <- bigchess::lan2san(moves)
      return(paste0(moves, ' 1/2-1/2 {fifty-move rule}'))
    }
    if (is_insufficient(boards[[ply]]$board)) {
      moves <- paste0(moves, ' ', bestmove)
      moves <- bigchess::lan2san(moves)
      return(paste0(moves, ' 1/2-1/2 {dead position}'))
    }
    if (is_repetition(boards, 3)) {
      moves <- paste0(moves, ' ', bestmove)
      moves <- bigchess::lan2san(moves)
      return(paste0(moves, ' 1/2-1/2 {threefold repetition}'))
    }
    if (!mute) {
      print(bestmove)
      print_board(boards[[ply]]$board)
    }
    moves <- paste0(moves, ' ', bestmove)
  }
  moves <- trimws(moves)
  moves <- bigchess::lan2san(moves)
  moves <- append_result(moves)
  return(moves)
}

#' Append outcome of chess game to moves in standard algebraic notation (SAN)
#'
#' The function `append_result()` is  helper function for autoplay_game() that
#' takes a string of chess moves in standard algebraic notation (SAN) as the
#' argument. If the last character is # it checks to see which side won, and
#' appends the appropriate result ("1-0" or "0-1").  If the last character is
#' not #, it assumes stalemate and adds "1/2-1/2 {stalemate}".
#'
#' @param movetext A string of chess moves in standard algebraic notation (SAN).
#'
#' @return A string of chess moves with the game result appended.
append_result <- function(movetext) {
  # Split the string into moves
  moves <- strsplit(movetext, "\\. ")[[1]]

  # Get the last move
  last_move <- utils::tail(moves, n = 1)

  # Check if the last move is by white or black
  is_white_move <- length(strsplit(last_move, " ")[[1]]) == 1

  # Check if the last character is #
  if (substr(last_move, nchar(last_move), nchar(last_move)) == "#") {
    # If the last move was by white, white won. Otherwise, black won.
    if (is_white_move) {
      movetext <- paste(movetext, "1-0")
    } else {
      movetext <- paste(movetext, "0-1")
    }
  } else {
    # If the last character is not #, the game was a draw.
    movetext <- paste(movetext, "1/2-1/2 {stalemate}")
  }

  return(movetext)
}

