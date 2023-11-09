#' Evaluate a chess position
#'
#' Chess positions are analyzed using a
#' [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#' engine.
#'
#' @param position A single-element character vector containing a series of
#'   legal chess moves in long algebraic notation (LAN).
#' @param engine_path A single-element character vector of the path to a UCI
#'    compatible chess engine.
#' @param limiter A single-element character vector indicating the desired
#'   mode of search termination. Allowed values are 'depth' (to search a fixed
#'   number of plies), 'nodes' (to search a fixed number of nodes), and
#'   'movetime' (to search for a fixed number of milliseconds).
#' @param limit A single-element integer vector of the desired search depth
#'   (# of plies), search nodes (# of nodes), or search time
#'   (# of milliseconds).
#' @param n_cpus (Default = 1) A single-element integer vector of the number of
#'    CPUs to use.
#' @param n_pv (Default = 1) A single-element integer vector of the desired
#'   number of principal variations.
#' @param hash_size (Default = NULL) A single-element integer vector of the
#'   desired hash size, in MB.
#'
#' @return A list containing a character vector of the engine's output.
#' @export
#'
#' @inherit tabulate_positionlog seealso
#'
#' @examples
#' position <- 'e2e4'
#' # Modify engine_path as required for your engine location & operating system
#' engine_path <- '//stockfish.exe'
#' evaluate_position(position, engine_path, n_pv = 1, n_cpus = 1,
#'                   limiter = 'depth', limit = 1)
evaluate_position <- function(position, engine_path, limiter, limit, n_cpus,
                              n_pv, hash_size = NULL) {
  # Validate the input
  assertthat::assert_that(assertthat::is.string(position))
  assertthat::assert_that(assertthat::is.string(engine_path))
  assertthat::assert_that(assertthat::is.count(n_cpus))
  assertthat::assert_that(assertthat::is.count(n_pv))
  assertthat::assert_that(assertthat::is.string(limiter))
  assertthat::assert_that(limiter %in% c('depth', 'nodes', 'movetime'))
  assertthat::assert_that(assertthat::is.count(limit))
  assertthat::assert_that(assertthat::is.count(hash_size) |
                          is.null(hash_size))
  # Set up the engine
  engine <- bigchess::uci_engine(engine_path)
  cpu_command <- paste0('setoption name Threads value ', n_cpus)
  engine <- bigchess::uci_cmd(engine, command = cpu_command)
  pv_command <- paste0('setoption name MultiPV value ', n_pv)
  engine <- bigchess::uci_cmd(engine, command = pv_command)
  if (!is.null(hash_size)) {
    hash_command <- paste0('setoption name Hash value ', hash_size)
    engine <- bigchess::uci_cmd(engine, command = hash_command)
  }
  engine <- bigchess::uci_ucinewgame(engine)
  engine <- bigchess::uci_isready(engine)
  engine <- bigchess::uci_position(engine)
  go_command <- paste('go', limiter, limit)
  # Evaluate the position & return evaluation
  move_command <- paste0('position startpos moves ', position)
  engine <- bigchess::uci_cmd(engine, command = move_command)
  engine <- bigchess::uci_cmd(engine, command = go_command)
  uci_bestmove <- ''
  while (length(grep("bestmove", uci_bestmove)) < 1) {
    engine <- bigchess::uci_read(engine)
    ucilog <- engine$temp
    uci_bestmove <- ucilog[length(ucilog)]
  }
  return(list(bigchess::uci_read(engine)$temp))
}
