#' Get all legal moves for the provided chess position.
#'
#' Generates a list of all legal moves from the provided chess position.
#'
#' @details The function `get_legal_moves()` passes the position to an external
#'   [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#'   engine, which must be supplied by the user. The path to the engine's
#'   executable file is given by the engine_path parameter. The engine is asked
#'   to return an analysis with minimum depth, and maximum number of principal
#'   variations (PVs). The legal moves are obtained by parsing the PV output. If
#'   the number of PVs given by the engine is limited, then it is possible that
#'   not every possible legal move will be generated.
#'
#' @details The position to be analyzed is represented as a sequence of legal
#'   chess moves. As a consequence, it is not possible to generate moves for
#'   positions that can not be arrived at by a sequence of legal moves.
#'
#' @param position A single-element character vector containing a sequence of
#'   moves in long algebraic notation (LAN).
#' @param engine_path A single-element character vector of the path to a UCI
#'    compatible chess engine.
#'
#' @return A character vector containing all of the legal moves in a position in
#'    long algebraic notation (LAN).
#' @export
#'
#' @examples
#' # Show all possible white first moves
#' #' # Modify engine_path as required for your engine location & operating system
#' engine_path <- '//stockfish.exe'
#' get_legal_moves('', engine_path)
get_legal_moves <- function(position, engine_path) {
  # Validate the input
  assertthat::assert_that(assertthat::is.string(position))
  assertthat::assert_that(assertthat::is.string(engine_path))  # Convert the game to a machine readable format
  # Set up the engine
  n_cpus <- 1L
  n_pv <- 500L
  limiter <- 'depth'
  limit <- 1L
  engine <- bigchess::uci_engine(engine_path)
  cpu_command <- paste0('setoption name Threads value ', n_cpus)
  engine <- bigchess::uci_cmd(engine, command = cpu_command)
  pv_command <- paste0('setoption name MultiPV value ', n_pv)
  engine <- bigchess::uci_cmd(engine, command = pv_command)
  engine <- bigchess::uci_ucinewgame(engine)
  engine <- bigchess::uci_isready(engine)
  engine <- bigchess::uci_position(engine)
  go_command <- paste('go', limiter, limit)
  # Analyze to depth 1
  move_command <- paste0('position startpos moves ', position)
  engine <- bigchess::uci_cmd(engine, command = move_command)
  engine <- bigchess::uci_cmd(engine, command = go_command)
  uci_bestmove <- ''
  while (length(grep("bestmove", uci_bestmove)) < 1) {
    engine <- bigchess::uci_read(engine)
    ucilog <- engine$temp
    uci_bestmove <- ucilog[length(ucilog)]
  }
  bigchess::uci_read(engine)$temp
  bigchess::uci_quit(engine)
  legal_moves <- parse_gamelog(list(ucilog), 'pv')
  return(unlist(legal_moves))
}
