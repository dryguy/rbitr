#' Analyze a chess game
#'
#' Chess games are analyzed using a
#' [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#' engine.
#'
#' @details The function `evaluate_game()` passes game positions to an external
#'   [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#'   engine, which must be supplied by the user. The path to the engine's
#'   executable file is given by the engine_path parameter. The number of CPU's
#'   to devote to the analysis is determined by the n_cpus parameter.
#'
#' @details The game to be analyzed should be in a PGN-compatible format, that
#'   is, moves should be in standard algebraic notation (SAN), and any comments,
#'   annotations, or variations should comply with the
#'   [PGN specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm).
#'   If variations are present, they will not be analyzed. The game is passed
#'   into `evaluate_game()` as a single-element character vector via the
#'   movetext parameter. The movetext is typically obtained from `get_pgn()`,
#'   where it would be found in the $Movetext column of `get_pgn()`'s output.
#'
#' @details The number of principal variations to be analyzed is set using the
#'   n_pv parameter, while the depth of analysis is controlled by choosing a
#'   UCI command to set the criteria for search termination as either 'depth'
#'   (to search a fixed  number of plies), 'nodes' (to search a fixed number of
#'   positions), or 'movetime' (to search a fixed number of milliseconds). The
#'   selection is controlled by the limiter and limit parameters. Note
#'   that the values apply to each position analyzed. For example, if the game
#'   has 80 plies, and 'movetime' is set to 1000 milliseconds, the analysis will
#'   take 80 seconds.
#'
#' @note The server analysis on lichess.org has a limit of 2250000 nodes. To
#'    mimic this, set limiter = 'nodes', and limit = 2250000.
#'
#' @param movetext A single-element character vector containing a sequence of
#'   moves in standard algebraic notation (SAN).
#' @param engine_path A single-element character vector of the path to a UCI
#'    compatible chess engine.
#' @param limiter A single-element character vector indicating the desired
#'   mode of search termination. Allowed values are 'depth' (to search a fixed
#'   number of plies), 'nodes' (to search a fixed number of nodes), and
#'   'movetime' (to search a fixed number of milliseconds).
#' @param limit A single-element integer vector of the desired search depth
#'   (# of plies), search nodes (# of nodes), or search time
#'   (# of milliseconds).
#' @param n_cpus (Default = 1) A single-element integer vector of the number of
#'    cpus to use.
#' @param n_pv (Default = 1) A single-element integer vector of the desired
#'   number of principal variations.
#' @param mute (Default = TRUE) A single-element boolean vector indicating
#'   whether to display progress.
#' @param hash_size (Default = NULL) A single-element integer vector of the
#'   desired hash size, in MB.
#'
#' @return A list containing character vectors of the engine's output. Each
#'   element in the list corresponds to a position in the game. The first entry
#'   in the list corresponds to the initial position of the game before any move
#'   has been made.
#' @export
#'
#' @seealso
#'   The 'cram' functions condense analysis logs into data frames.
#'   * [rbitr::cram_positionlog()] for condensing analysis of one position.
#'   * [rbitr::cram_pgnlog()] for condensing analysis of an entire pgn file.
#'
#'   The 'parse' functions extract specific data from analysis logs.
#'   * [rbitr::parse_gamelog()] for extracting data from one evaluated game.
#'   * [rbitr::parse_pgnlog()] for extracting data from games in a pgn.
#'
#'   The 'evaluate' functions produce analysis logs.
#'   * [rbitr::evaluate_position()] for analyzing chess positions.
#'   * [rbitr::evaluate_pgn()] for evaluating all the games in a PGN file.
#'
#'   Functions to load and save PGN files.
#'   * [rbitr::get_pgn()] for loading PGN files.
#'   * [rbitr::save_pgn()] for saving PGN files.
#'
#' @examples
#' movetext <- '1. e4 g5 2. Nc3 f5 3. Qh5# 1-0'
#' # Modify engine_path as required for your engine location & operating system
#' engine_path <- '//stockfish.exe'
#' evaluate_game(movetext, engine_path, n_pv = 1, limiter = 'depth', limit = 1)

evaluate_game <- function(movetext, engine_path, limiter, limit,
                          n_cpus = 1L, n_pv = 1L, mute = TRUE,
                          hash_size = NULL) {
  # Validate the input
  assertthat::assert_that(assertthat::is.string(movetext))
  assertthat::assert_that(assertthat::is.string(engine_path))
  assertthat::assert_that(assertthat::is.count(n_cpus))
  assertthat::assert_that(assertthat::is.count(n_pv))
  assertthat::assert_that(limiter == 'depth' |
                          limiter == 'nodes' |
                          limiter == 'movetime')
  assertthat::assert_that(assertthat::is.count(limit))
  assertthat::assert_that(assertthat::is.flag(mute))
  assertthat::assert_that(assertthat::is.count(hash_size) |
                          is.null(hash_size))
  # Convert the game to a machine readable format
  moves <- clean_movetext(movetext)
  moves <- bigchess::san2lan(moves)
  moves <- unlist(strsplit(moves, split = ' '), use.names = FALSE)
  moves <- c('', moves)
  n_moves <- length(moves)
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
  # Analyze the game
  analyze_position <- function(move_index, moves, engine, go_command, n_moves,
                           mute) {
    if (!mute) {
      print(paste0('Analyzing position ', move_index, ' of ', n_moves))
    }
    moves <- paste0(moves[1:move_index], collapse = ' ')
    move_command <- paste0('position startpos moves ', moves)
    engine <- bigchess::uci_cmd(engine, command = move_command)
    engine <- bigchess::uci_cmd(engine, command = go_command)
    uci_bestmove <- ''
    while (length(grep('bestmove', uci_bestmove)) < 1) {
      engine <- bigchess::uci_read(engine)
      ucilog <- engine$temp
      uci_bestmove <- ucilog[length(ucilog)]
    }
    bigchess::uci_read(engine)$temp
  }
  move_index <- 1:n_moves
  result <- lapply(move_index, analyze_position, moves = moves, engine = engine,
         go_command = go_command, n_moves = n_moves, mute = mute)
  bigchess::uci_quit(engine)
  return(result)
}
