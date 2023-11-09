#' Creates a list of data frames containing analyses of chess games
#'
#' The function `tabulate_pgnlog()`  takes a list of engine analyses for chess games
#'   and condenses them into data frames for easier access.
#'
#' @details `tabulate_pgnlog()` condenses analyses from a
#'   [UCI-compatible](https://github.com/fsmosca/UCIChessEngineProtocol) chess
#'   engine that have been produced using the `evaluate_pgn()` function.
#'
#' @details `tabulate_pgnlog()` is a wrapper for `tabulate_gamelog()` that loops
#'   over each game analysis in the pgnlog and creates a data frame containing
#'   the condensed analysis data. The `pgnlog` parameter should contain output
#'   from the `evaluate_pgn()` function. The remaining parameters are passed to
#'   `tabulate_gamelog()`. See the documentation for [rbitr::tabulate_positionlog()] and
#'   [rbitr::tabulate_gamelog()] for details.
#'
#' @param pgnlog A list of gamelogs from rbitr's `evaluate_pgn()` function.
#' @inheritParams tabulate_positionlog
#'
#' @return A list of data frames containing condensed analyses of chess games.
#'
#' @export
#'
#' @inherit tabulate_positionlog seealso
#'
#' @examples
#' pgnlog <- list(list(c(
#' "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
#' "readyok",
#' "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
#' "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
#' "bestmove d2d4"
#' )))
#' tabulate_pgnlog(pgnlog)
tabulate_pgnlog <- function(pgnlog, all_tags = FALSE, custom_tags = NULL,
                        delete_blank_lines = TRUE) {
  # Validate input
  assertthat::assert_that(is.list(pgnlog))
  assertthat::assert_that(assertthat::is.flag(all_tags))
  assertthat::assert_that(is.character(custom_tags) | is.null(custom_tags))
  assertthat::assert_that(assertthat::is.flag(delete_blank_lines))

  # Condense the data into a list of data frames.
  lapply(pgnlog, tabulate_gamelog, all_tags, custom_tags, delete_blank_lines)
}

#' Deprecated alias for [rbitr::tabulate_pgnlog()]
#' @inherit tabulate_pgnlog
#' @export
cram_pgnlog <- function(pgnlog, all_tags = FALSE, custom_tags = NULL,
                        delete_blank_lines = TRUE) {
  tabulate_pgnlog(pgnlog, all_tags, custom_tags, delete_blank_lines)
}
