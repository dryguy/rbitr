#' Creates a data frame containing engine analysis of a chess game
#'
#' The `cram_gamelog()` function takes a list of vectors of engine
#'   analysis for a chess game and condenses it into a data frame for
#'   easier access.
#'
#' @details The function `cram_gamelog()` parses output from a
#'   [UCI](https://github.com/fsmosca/UCIChessEngineProtocol)-compatible chess
#'   engine.
#'
#' @details `cram_gamelog()` is a wrapper for `cram_positionlog()` that loops
#'   over each position in the game, and adds a column indicating the position
#'   number. The `gamelog` parameter should contain output from the
#'   `evaluate_game()` function. The remaining parameters are passed to
#'   `cram_positionlog()`. See the documentation for [rbitr::cram_positionlog()]
#'   for details.
#'
#' @param gamelog A list of vectors of engine analysis
#' @param all (Default = FALSE) A boolean. Setting `all` = TRUE will search for
#'   all of the info tokens listed in the UCI protocol
#' @param patterns (Default = NULL) An optional character vector of one or more
#'   user-supplied regular expressions
#' @param column_names (Default = NULL) An optional character vector of column
#'   names for data extracted by the user-supplied regular expressions
#' @param delete_blank_lines (Default = TRUE) A boolean. Setting this value to
#'   FALSE will leave blank rows/columns intact
#' @return A data frame summarizing the data for the game
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
#'   * [rbitr::evaluate_game()] for analyzing individual games.
#'   * [rbitr::evaluate_pgn()] for evaluating all the games in a PGN file.
#'
#' @examples
#' gamelog <- list(c(
#'   "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
#'   "readyok",
#'   "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
#'   "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
#'   "bestmove d2d4"
#' ))
#' cram_gamelog(gamelog)
cram_gamelog <- function(gamelog, all = FALSE, patterns = NULL,
                         column_names = NULL, delete_blank_lines = TRUE) {
  # Validate input
  assertthat::assert_that(is.list(gamelog))
  assertthat::assert_that(assertthat::is.flag(all))
  assertthat::assert_that(is.character(patterns) | is.null(patterns))
  assertthat::assert_that(is.character(column_names) | is.null(column_names))
  assertthat::assert_that(length(column_names) == length(patterns))
  assertthat::assert_that(assertthat::is.flag(delete_blank_lines))

  # Cram the gamelog
  position_indices <- 1:length(gamelog)
  cram_positionlogs <- function(position_index, gamelog, all, patterns,
                                column_names, delete_blank_lines) {
    positionlog <- gamelog[[position_index]]
    crammed_positionlog <- cram_positionlog(positionlog, all, patterns,
                                            column_names, delete_blank_lines)
    position <- rep(position_index, nrow(crammed_positionlog))
    crammed_positionlog <- cbind(position, crammed_positionlog)
  }
  crammed_gamelog <- lapply(position_indices, cram_positionlogs, gamelog, all,
                            patterns, column_names, delete_blank_lines)
  crammed_gamelog <- do.call(rbind, crammed_gamelog)
  return(crammed_gamelog)
}
