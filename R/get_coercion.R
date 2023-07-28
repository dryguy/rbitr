#' Gets the coercion of a chess position
#'
#' The `get_coercion()` function calculates the difference in evaluation between
#'   the best and second best moves in a chess position, referred to here as the
#'   `coercion` of the position.
#'
#' @details In chess, *forcing* moves are moves that give the opponent only a
#'   small number of ways to avoid an unfavorable position. Guid and Bratko (see
#'   references) used the difference in engine evaluation between the best and
#'   second best moves in a position as a measure of how forcing a position is.
#'
#' @details Given a log of engine analysis of a chess position produced by one
#'   of `rbitr`'s evaluation functions, `get_coercion()` will compute the
#'   difference in evaluation between the best and second best moves.
#'
#' @details `get_coercion()` requires as input a `positionlog` of engine
#'   analysis. The analysis must contain at least two principal variations
#'   (`pv`'s) to allow the `coercion` to be determined. If only one PV is
#'   provided, or if no move is possible (i.e., in positions of mate or
#'   stalemate) it will return `Inf`. The highest depth of search available will
#'   be used by default.
#'
#' @note When only one PV is available, this may be because there is only one
#'   legal move, or because the engine only analyzed one PV. When more than one
#'   legal move exists, but the engine only analyzed one PV, it might make more
#'   sense to consider the coercion to be NA. However, at present,
#'   `get_coercion()` can't distinguish between these two cases and returns
#'   `Inf` for either.
#'
#' @param positionlog A character vector of engine analysis
#' @param depth A single-element integer vector of the desired search depth
#'   (# of plies)
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
get_coercion <- function(positionlog, depth = NULL) {
  # Validate input
  assertthat::assert_that(is.character(positionlog))
  assertthat::assert_that(assertthat::is.count(depth) | is.null(depth))
  # Extract data from the positionlog
  crammed_positionlog <- cram_positionlog(positionlog)
  # Check that multipv is present
  if (is.null(crammed_positionlog$multipv)) {return(NA)}
  # Convert numeric columns
  crammed_positionlog$depth   <- as.numeric(crammed_positionlog$depth)
  crammed_positionlog$multipv <- as.numeric(crammed_positionlog$multipv)
  # Check for at least 2 pvs
  if (max(crammed_positionlog$multipv, na.rm=TRUE) < 2) {return(Inf)}
  # Use maximum depth?
  if(is.null(depth)) {
    depth <- max(crammed_positionlog$depth, na.rm=TRUE)
  }
  # Extract top 2 scores at max_depth
  scores <- crammed_positionlog$score[crammed_positionlog$depth == depth &
                                      crammed_positionlog$multipv %in% c(1, 2)]
  # Convert evaluations to numeric
  scores <- convert_scores(scores, mate = Inf, flip_signs = FALSE)
  # Return coercion at max depth
  if(all(scores == Inf) || all(scores == -Inf)) {
    return(0)
  }
  return(scores[1] - scores[2])
}

