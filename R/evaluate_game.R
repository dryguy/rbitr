#' Analyze a chess game.
#'
#' Chess games are analyzed using a
#' [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#' engine.
#'
#' @details The rbitr package relies on the
#'   [bigchess](https://github.com/rosawojciech/bigchess) package to handle the
#'   interfaces to
#'   [Universal Chess Interface (UCI)](http://wbec-ridderkerk.nl/html/UCIProtocol.html)
#'   compatible chess engines. To use `evaluate_game()`, an engine handler must
#'   first be created with the bigchess `uci_engine()` function, and the handler
#'   must then be passed to `evaluate_game()` (see example).
#'
#' @details The game to be analyzed should be in a pgn-compatible format, that
#'   is, moves should be in standard algebraic notation (SAN) and any comments,
#'   annotations, or variations should comply with the
#'   [pgn specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm).
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
#'   selection is controlled by the go_command and go_value parameters. Note
#'   that the values apply to each position analyzed. For example, if the game
#'   has 80 plies, and 'movetime' is set to 1000 milliseconds, the analysis will
#'   take 80 seconds.
#'
#' @note The server analysis on lichess.org use a limit of 2250000 nodes. To
#'    mimic this, set go_mode = 'nodes', and go_value = 2250000.
#'
#' @param movetext A single-element character vector containing a sequence of
#'   moves in standard algebraic notation (SAN).
#' @param engine_path A single-element character vector of the path to a UCI
#'    chess engine.
#' @param n_cpus A single-element integer vector of the number of cpus to use.
#' @param n_pv A single-element integer vector of the desired number of
#'   principal variations.
#' @param go_mode A single-element character vector indicating the desired
#'   mode of search termination. Allowed values are 'depth' (to search a fixed
#'   number of plies), 'nodes' (to search a fixed number of nodes), and
#'   'movetime' (to search a fixed number of milliseconds).
#' @param go_value A single-element integer vector of the desired search depth
#'   (# of plies), search nodes (# of nodes), or search time (# of mseconds).
#'
#' @return A list containing character vectors of the engine output. Each
#'   element in the list corresponds to a position in the game, beginning with
#'   the initial position before any move has been made.
#' @export
#'
#' @examples
#' library(bigchess)
#' movetext <- '1. e4 g5 2. Nc3 f5 3. Qh5# 1-0'
#' # Modify engine_path as required for your engine location & operating system
#' engine_path <- '//stockfish_13_win_x64_bmi2.exe'
#' evaluate_game(movetext, engine_path, n_pv = 1, go_mode = 'depth', go_value = 1)

evaluate_game <- function(movetext, engine_path, n_cpus = 1, n_pv, go_mode,
                          go_value) {
  # Validate the input
  assertthat::assert_that(assertthat::is.string(movetext))
  assertthat::assert_that(assertthat::is.string(engine_path))
  assertthat::assert_that(assertthat::is.count(n_cpus))
  assertthat::assert_that(assertthat::is.count(n_pv))
  assertthat::assert_that(go_mode == 'depth' |
                          go_mode == 'nodes' |
                          go_mode == 'movetime')
  assertthat::assert_that(assertthat::is.count(go_value))
  # Convert the game to a machine readable format
  moves <- clean_movetext(movetext)
  moves <- bigchess::san2lan(moves)
  moves <- unlist(strsplit(moves, split = ' '), use.names = FALSE)
  moves <- c('', moves)
  # Set up the engine
  engine <- bigchess::uci_engine(engine_path)
  cpu_command <- paste0('setoption name Threads value ', n_cpus)
  engine <- bigchess::uci_cmd(engine, command = cpu_command)
  pv_command <- paste0('setoption name MultiPV value ', n_pv)
  engine <- bigchess::uci_cmd(engine, command = pv_command)
  engine <- bigchess::uci_ucinewgame(engine)
  engine <- bigchess::uci_isready(engine)
  engine <- bigchess::uci_position(engine)
  go_command <- paste('go', go_mode, go_value)
  # Analyze the game
  analyze_move <- function(move_index, moves, engine, go_command) {
    moves <- paste0(moves[1:move_index], collapse = ' ')
    move_command <- paste0('position startpos moves ', moves)
    engine <- bigchess::uci_cmd(engine, command = move_command)
    engine <- bigchess::uci_cmd(engine, command = go_command)
    rr <- ''
    while (length(grep("bestmove", rr)) < 1) {
      engine <- bigchess::uci_read(engine)
      rl <- engine$temp
      rr <- rl[length(rl)]
    }
    bigchess::uci_read(engine)$temp
  }
  move_index <- 1:length(moves)
  result <- lapply(move_index, analyze_move, moves = moves, engine = engine,
         go_command = go_command)
  bigchess::uci_quit(engine)
  return(result)
}
