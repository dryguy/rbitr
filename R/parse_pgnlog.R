#' Extract data from rbitr's `evaluate_pgn()` output
#'
#' The `parse_pgnlog()` function is a wrapper for the `parse_gamelog()` function
#'   that iterates over all the games in a pgn.
#'
#' @details The output of rbitr's `evaluate_pgn()` function is a list of logs
#'   from the `evaluate_game()` function, where each entry in the list is for a
#'   single game. The `parse_pgnlog()` function can extract  evaluations in
#'   centipawns (score), principal variations (pv), or best moves (bestmove),
#'   depending on the setting for the `target` parameter.
#'
#' @details See [rbitr::parse_gamelog()] for more details.
#'
#' @param pgnlog A list of gamelogs from rbitr's `evaluate_pgn()` function.
#' @param target A single-element character vector of the output to return.
#'   Allowed values are 'score' for the evaluation in centipawns, 'pv' for the
#'   principal variation, or 'bestmove' for the best move.
#' @param depth (Optional, default = NULL) A single-element integer vector
#'   indicating which search depth to return. The value must not be less than 1
#'   or greater than the maximum depth reported by the engine. A value of NULL
#'   returns data for the maximum depth.
#'
#' @return A list of vectors of the extracted data.
#' @export
#'
#' @seealso
#'   * [rbitr::parse_gamelog()] for extracting data from one evaluated game.
#'   * [rbitr::evaluate_game()] for analyzing individual games.
#'   * [rbitr::evaluate_pgn()] for evaluating all the games in a pgn file.
#'
#' @examples
#' pgnlog <- list(list(c(
#' "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
#' "readyok",
#' "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
#' "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
#' "bestmove d2d4"
#' )))
#' parse_pgnlog(pgnlog, 'score')
#' parse_pgnlog(pgnlog, 'pv')
#' parse_pgnlog(pgnlog, 'bestmove')
parse_pgnlog <- function(pgnlog, target, depth = NULL) {
  # Validate input
  assertthat::assert_that(is.list(pgnlog))
  assertthat::assert_that(assertthat::is.count(depth) | is.null(depth))
  assertthat::assert_that(target == 'score' |
                          target == 'pv'    |
                          target == 'bestmove')
  # Parse the log
  lapply(pgnlog, parse_gamelog, target, depth)
}
