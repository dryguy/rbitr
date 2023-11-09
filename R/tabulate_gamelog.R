#' Creates a data frame containing engine analysis of a chess game
#'
#' The `tabulate_gamelog()` function takes a list of vectors of engine analysis
#' for a chess game and condenses it into a data frame for easier access.
#'
#' @details The function `tabulate_gamelog()` parses output from a
#'   [UCI-compatible](https://github.com/fsmosca/UCIChessEngineProtocol) chess
#'   engine.
#'
#' @details `tabulate_gamelog()` is a wrapper for `tabulate_positionlog()` that
#'   loops over each position in the game, and adds a column indicating the
#'   position number. The `gamelog` parameter should contain output from the
#'   `evaluate_game()` function. The remaining parameters are passed to
#'   `tabulate_positionlog()`. See the documentation for
#'   [rbitr::tabulate_positionlog()] for details.
#'
#' @param gamelog A list that contains vectors of engine analysis.
#' @inheritParams tabulate_positionlog
#'
#' @return A data frame summarizing the data for the game.
#'
#' @export
#'
#' @inherit tabulate_positionlog seealso
#'
#' @examples
#' gamelog <- list(c(
#'   "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
#'   "readyok",
#'   "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
#'   "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
#'   "bestmove d2d4"
#' ))
#' tabulate_gamelog(gamelog)
tabulate_gamelog <- function(gamelog, all_tags = FALSE, custom_tags = NULL,
                         delete_blank_lines = TRUE) {
  # Validate input
  assertthat::assert_that(is.list(gamelog))
  assertthat::assert_that(assertthat::is.flag(all_tags))
  assertthat::assert_that(is.character(custom_tags) | is.null(custom_tags))
  assertthat::assert_that(assertthat::is.flag(delete_blank_lines))

  # Tabulate the gamelog
  position_indices <- 1:length(gamelog)
  tabulate_positionlogs <- function(position_index, gamelog, all_tags,custom_tags,
                                delete_blank_lines) {
    positionlog <- gamelog[[position_index]]
    positionlog_table <- tabulate_positionlog(positionlog, all_tags, custom_tags,
                                            delete_blank_lines)
    position <- rep(position_index, nrow(positionlog_table))
    positionlog_table <- cbind(position, positionlog_table)
  }
  gamelog_table <- lapply(position_indices, tabulate_positionlogs, gamelog,
                            all_tags, custom_tags, delete_blank_lines)
  gamelog_table <- harmonize_columns(gamelog_table)
  gamelog_table <- do.call(rbind, gamelog_table)
  return(gamelog_table)
}

#' Deprecated alias for [rbitr::tabulate_gamelog()]
#' @inherit tabulate_gamelog
#' @export
cram_gamelog <- function(gamelog, all_tags = FALSE, custom_tags = NULL,
                         delete_blank_lines = TRUE) {
  tabulate_gamelog(gamelog, all_tags, custom_tags, delete_blank_lines)
}
