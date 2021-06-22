#' Calculate the average centipawn loss
#'
#' A function to calculate average centipawn loss (ACPL) using any of several
#'   different definitions.
#'
#' @details The function `get_acpl()` calculates the average centipawn loss
#'   (ACPL) for the side indicated by the `color` parameter. The `scores`
#'   parameter is assumed to begin with a score for the initial position, prior
#'   to the first move. Note that PGN files do not store an evaluation of the
#'   initial position, whereas chess engines usually will provide an evaluation
#'   of the initial position. When using data from a PGN file, it is up to the
#'   user to supply a score for the initial position before calling `get_acpl()`.
#'
#' @details Evaluations may be capped to avoid having blunders or missed mates
#'   result in excessively large ACPL. If the `cap` parameter is set, then
#'   evaluations outside the range \[-cap, cap\] will be handled as indicated
#'   by the `cap_action` parameter.
#'
#' @details For positions evaluated as 'mate in x', it is up to the user to
#'   convert these to numeric scores before calling `get_acpl()`. In general,
#'   if a cap will be applied to evaluations, it is only necessary to convert
#'   'mate x' or 'mate -x' to values outside the range \[-cap, cap\]. The rbitr
#'   function `convert_scores()` may be used for this conversion.
#'
#' @details The method for calculating average centipawn loss varies. The
#'   concept of average centipawn loss was introduced by Guid & Bratko (which
#'   they called mean loss). It was calculated only after excluding the first
#'   11 moves, and also after excluding moves where both the best move and the
#'   played move result in evaluations outside the range \[-200, 200\]. To
#'   replicate Guid & Bratko's MeanLoss, set `cap` to 200, `cap_action` to
#'   'exclude' and, `first_ply` to 23. (The ply count starts at 1 for the
#'   initial position, just before the first move, so the position before
#'   white's nth move always occurs at ply 2n - 1.)
#'
#' @details The website [lichess.org](lichess.org) uses a different method, in which
#'   evaluations outside the range \[-1000, 1000\] are replaced with +-1000, and
#'   the initial position is always given a score of 15. To replicate the
#'   lichess ACPL, set `cap` to 1000, `cap_action` to 'replace', and make sure
#'   that the first element of the `scores` parameter is an evaluation of the
#'   initial position and is equal to 15.
#'
#' @details Regan, _et_ _al._, use yet another method to calculate ACPL, which
#'   they refer to as average error (AE). They exclude evaluations outside the
#'   range \[-300, 300\], and turns 1-8. They also exclude positions that are
#'   repetitions. However, rbitr does not currently have the ability to detect
#'   repetitions. To approximate AE without accounting for repetitions, set
#'   `cap` to 300, `cap_action` to exclude, and `first_ply` to 17.
#'
#' @note For games that use alternative starting positions, or for partial
#'   games, it is possible for black to have the first move. In these
#'   situations, set the `to_move` parameter to 'black'.
#'
#' @param scores A numeric vector of evaluations, in centipawns.
#' @param color A single-element character vector indicating which side to
#'   evaluate. Allowed values are 'black' or 'white'.
#' @param cap (Default = NULL) A single-element numeric vector indicating the
#'   threshold for capping a move's loss (or excluding the move from the
#'   average).
#' @param cap_action (Default = NULL) A single-element character vector
#'   indicating whether to exclude scores outside the range \[-cap, cap\], or to
#'   replace them with the cap. Allowed values are 'exclude', 'replace', or
#'   'none'.
#' @param first_ply (Default = 1) A single-element integer vector indicating the
#'   first ply to be included in the calculation. May not be larger than the
#'   number of elements in `scores`.
#' @param to_move (Default = 'white') A single-element character vector
#'   indicating which side's turn it is.
#'
#' @return A single-element integer vector of the average centipawn loss.
#' @export
#'
#' @references
#'   * Guid, M., & Bratko, I. (2006). Computer Analysis of World Chess
#'     Champions. ICGA Journal, 29(2), 65-73.
#'   * Regan, K. W., Macieja, B., & Haworth, G. M. C. (2012). Understanding
#'     distributions of chess performances. Lecture Notes in Computer Science,
#'     7168, 230-243. https://doi.org/10.1007/978-3-642-31866-5_20
#'
#' @seealso
#'   * [rbitr::get_imb()] to determine inaccuracies, mistakes, and blunders.
#'   * [rbitr::get_evals()] to load scores from a PGN file.
#'   * [rbitr::evaluate_game()] or [rbitr::evaluate_pgn()] to calculate scores.
#'   * [rbitr::convert_scores()] to set values for 'mate x'.
#'
#' @examples
#' scores <- c(15, 5, 29, -94, 67, 76, 154, -31, 1000, 1000)
#' get_acpl(scores, 'white')
get_acpl <- function(scores, color, cap = NULL, cap_action = 'none',
                     first_ply = 1, to_move = 'white') {
  # Validate input
  assertthat::assert_that(is.numeric(scores))
  assertthat::assert_that(color == 'white' | color == 'black')
  assertthat::assert_that(is.numeric(cap) | is.null(cap))
  assertthat::assert_that(length(cap) == 1 | length(cap) == 0)
  assertthat::assert_that(cap_action == 'exclude' |
                          cap_action == 'replace' |
                          cap_action == 'none')
  assertthat::assert_that(assertthat::is.count(first_ply))
  assertthat::assert_that(max(first_ply, na.rm = TRUE) <= length(scores))
  assertthat::assert_that(assertthat::is.count(min(first_ply, na.rm = TRUE)))
  assertthat::assert_that(to_move == 'white' | to_move == 'black')
  if (cap_action == 'replace' | cap_action == 'exclude') {
    assertthat::assert_that(!is.null(cap))
  }
  # Whose turn is it?
  if (to_move == 'black') {
    scores <- c(NA, scores)
  }
  # Impose the cap, if any
  if (cap_action == 'replace') {
    scores[scores >  cap] <-  cap
    scores[scores < -cap] <- -cap
  } else if (cap_action == 'exclude') {
    scores[scores >  cap] <- NA
    scores[scores < -cap] <- NA
  }
  # Calculate mean loss
  cpl <- diff(scores)
  ply <- first_ply:length(cpl)
  white_ply <- ply[ply %% 2 == 1]
  black_ply <- ply[ply %% 2 == 0]
  if (color == 'white') {
    white_cpl <- cpl[white_ply]
    white_cpl[white_cpl >= 0] <- 0
    return(floor(-mean(white_cpl, na.rm = TRUE)))
  } else if (color == 'black') {
    black_cpl <- cpl[black_ply]
    black_cpl[black_cpl <= 0] <- 0
    return(floor(mean(black_cpl, na.rm = TRUE)))
  }
}
