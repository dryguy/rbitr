#' Creates a list of data frames containing analyses of chess games
#'
#' The function `cram_pgnlog()`  takes a list of engine analyses for chess games
#'   and condenses them into data frames for easier access.
#'
#' @details `cram_pgnlog()` condenses analyses from a
#'   [UCI](https://github.com/fsmosca/UCIChessEngineProtocol)-compatible chess
#'   engine that have been produced using the `evaluate_pgn()` function.
#'
#' @details `cram_pgnlog()` is a wrapper for `cram_gamelog()` that loops
#'   over each game analysis in the pgnlog and creates a data frame containing
#'   the condensed analysis data. The `pgnlog` parameter should contain output
#'   from the `evaluate_pgn()` function. The remaining parameters are passed to
#'   `cram_gamelog()`. See the documentation for [rbitr::cram_positionlog()] and
#'   [rbitr::cram_gamelog()] for details.
#'
#' @param pgnlog A list of gamelogs from rbitr's `evaluate_pgn()` function.
#' @param all (Default = FALSE) A boolean. Setting `all` = TRUE will search for
#'   all of the info tokens listed in the UCI protocol
#' @param patterns (Default = NULL) An optional character vector of one or more
#'   user-supplied regular expressions
#' @param column_names (Default = NULL) An optional character vector of column
#'   names for data extracted by the user-supplied regular expressions
#' @param delete_blank_lines (Default = TRUE) A boolean. Setting this value to
#'   FALSE will leave blank rows/columns intact
#'
#' @return A list of data frames containing condensed analyses of chess games.
#' @export
#'
#' @seealso
#'   The 'cram' functions are for condensing analysis logs into data frames.
#'   * [rbitr::cram_positionlog()] for condensing analysis of one position.
#'   * [rbitr::cram_gamelog()] for condensing analysis of one game.
#'   The 'parse' functions are for extracting specific data from analysis logs.
#'   * [rbitr::parse_gamelog()] for extracting data from one evaluated game.
#'   * [rbitr::parse_pgnlog()] for extracting data from games in a pgn.
#'   The 'evaluate' functions are for producing the analysis logs.
#'   * [rbitr::evaluate_game()] for analyzing individual games.
#'   * [rbitr::evaluate_pgn()] for evaluating all the games in a PGN file.
#'
#' @examples
#' pgnlog <- list(list(c(
#' "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
#' "readyok",
#' "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
#' "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
#' "bestmove d2d4"
#' )))
#' cram_pgnlog(pgnlog)
cram_pgnlog <- function(pgnlog, all = FALSE, patterns = NULL,
                        column_names = NULL, delete_blank_lines = TRUE) {
  # Validate input
  assertthat::assert_that(is.list(pgnlog))

  # Condense the data into a list of data frames.
  lapply(pgnlog, cram_gamelog, all, patterns, column_names, delete_blank_lines)
}
