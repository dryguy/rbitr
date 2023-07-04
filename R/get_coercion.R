#' Gets the coercion of a chess position
#'
#' The `get_coercion()` function calculates the difference in evaluation between
#'   the best and second best moves in a chess position, referred to as the
#'   `coercion` of the position.
#'
#' @details In chess, forcing moves are those moves in which the opponent has
#'   only a small number of choices that don't result in an unacceptable loss of
#'   material or positional advantage. Guid and Bratko (see references) used the
#'   difference in engine evaluation between the best and second best moves in a
#'   position as a measure of how forcing a position is.
#'
#' @details Given a log of engine analysis of a chess position produced by one
#'   of `rbitr`'s evaluation functions, `get_coercion()` will compute the
#'   difference in evaluation between the best and second best moves, referred
#'   to here as the `coercion` of the position.
#'
#' @details `get_coercion()` requires as input a `positionlog` of engine
#'   analysis. The analysis must contain at least two principal variations
#'   (`pv`'s) to allow the `coercion` to be determined. The highest depth of
#'   search available will be used by default.
#'
#' @param positionlog A character vector of engine analysis
#'
#' @return A single-element numeric vector of the `coercion` for the position.
#' @export
#'
#' @seealso
#'   The 'evaluate' functions produce analysis logs.
#'   * [rbitr::evaluate_position()] for analyzing chess positions.
#'   * [rbitr::evaluate_game()] for analyzing individual games.
#'   * [rbitr::evaluate_pgn()] for evaluating all the games in a PGN file.
#'
#' @references
#'   Guid, M.; Bratko, I. Computer Analysis of World Chess Champions. ICGA
#'     Journal 2006, 29 (2), 65–73.
#'
#' @examples
#' positionlog <- c(
#'   "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
#'   "readyok",
#'   "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
#'   "info depth 1 seldepth 1 multipv 1 score cp 161 nodes 59 nps 59000 tbhits 0 time 1 pv b1c3",
#'   "info depth 1 seldepth 1 multipv 2 score cp 146 nodes 59 nps 59000 tbhits 0 time 1 pv d2d4",
#'   "info depth 2 seldepth 2 multipv 1 score cp 195 nodes 147 nps 73500 tbhits 0 time 2 pv d2d4 g8f6",
#'   "info depth 2 seldepth 2 multipv 2 score cp 170 nodes 147 nps 73500 tbhits
#'   0 time 2 pv h2h4 g8f6 h4g5",
#'   "bestmove d2d4 ponder g8f6"
#' )
get_coercion <- function(positionlog) {
  # Validate input
  assertthat::assert_that(is.character(positionlog))
  # Extract data from the positionlog
  crammed_positionlog <- cram_positionlog(positionlog)
  # Check for at least 2 pvs
  assertthat::assert_that(max(crammed_positionlog$multipv, na.rm=TRUE) >= 2)
  # What is the maximum depth?
  max_depth <- max(crammed_positionlog$depth, na.rm=TRUE)
  # Extract top 2 scores at max_depth
  scores <- crammed_positionlog$score[crammed_positionlog$depth == max_depth &
                                      crammed_positionlog$multipv %in% c(1, 2)]
  # Convert evaluations to numeric
  scores <- convert_scores(scores, flip_signs = FALSE)
  # Return coercion at max depth
  return(-diff(scores))
}
