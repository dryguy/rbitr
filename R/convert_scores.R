#' Convert mates and bounds to numeric values
#'
#' A function to convert "mate x", "x upperbound", and "x lowerbound" into
#'   numeric values (in centipawns).
#'
#' @details [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html)
#'   chess engines return positional evaluations in centipawns. The UCI protocol
#'   also allows for evaluations to be expressed as upper or lower bounds. In
#'   positions where the engine finds a mate in some number of moves, the
#'   evaluation is given as "mate x" instead of a centipawn value. In addition,
#'   the UCI convention is that scores for white are given with the opposite
#'   sign from the scores for black (*i.e.*, a positive score for black means
#'   black is ahead, while a positive score for white means white is ahead.)
#'
#' @details The function `convert_scores()` will convert bounds or mates into
#'   numeric values. Values of "x upperbound" or "x lowerbound" will simply be
#'   converted to the numeric value of x. Values of "mate x" will be converted
#'   to the value specified by the mate parameter, in centipawns. The sign of
#'   black's scores will be reversed from the UCI convention, so that positive
#'   evaluations will always mean white is ahead, and negative will always mean
#'   black is ahead. The final scores will be returned as an integer vector.
#'
#' @note The function `convert_scores()` assumes that the scores begin with a
#'   position where white is to move.
#'
#' @param scores A character vector of scores from a
#'   [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#'   engine.
#' @param mate (Default = 5000) A single-element integer vector of the value to
#'   use for 'mate x'.
#'
#' @return An numeric vector of scores (in centipawns).
#' @export
#'
#' @seealso [rbitr::parse_gamelog()] to extract scores from engine output.
#'
#' @examples
#' scores <- c("90", "-26", "26 upperbound", "mate 1", "mate -1", "mate 0", NA)
#' convert_scores(scores)

convert_scores <- function(scores, mate = 5000) {
  # Validate input
  assertthat::assert_that(is.character(scores))
  assertthat::assert_that(is.numeric(mate) & length(mate) == 1)
  # Convert 'mate x' to numeric value
  mate0 <- which(stringr::str_detect(scores, 'mate 0'))
  if (length(mate0) > 0) {
    scores[mate0] <- -mate
  }
  scores[stringr::str_detect(scores, 'mate [0-9]+')] <- mate
  scores[stringr::str_detect(scores, 'mate -[0-9]+')] <- -mate
  # Convert upperbound & lowerbound to numeric
  bounds <- stringr::str_detect(scores, 'bound')
  if (any(bounds, na.rm = TRUE)) {
    scores[which(bounds)] <- stringr::str_replace(scores[which(bounds)],
                                                  ' [a-z]*bound', '')
  }
  # Convert characters to integer
  scores <- as.integer(scores)
  # Flip signs for black scores so that if score > 0 white is winning.
  black_index <- 1:length(scores) %% 2 == 0
  scores[black_index] <- -scores[black_index]
  scores
}
