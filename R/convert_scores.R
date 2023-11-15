#' Convert mates and bounds to numeric values
#'
#' A function to convert "mate x", "x upperbound", and "x lowerbound" into
#' numeric values (in centipawns).
#'
#' @details [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html)
#'   chess engines return positional evaluations in centipawns. The UCI protocol
#'   also allows for evaluations to be expressed as upper or lower bounds. In
#'   positions where the engine finds a mate in some number of moves, the
#'   evaluation is given as "mate x" instead of a centipawn value.
#'
#'   The function `convert_scores()` will convert bounds or mates into numeric
#'   values. Values of "x upperbound" or "x lowerbound" will simply be converted
#'   to the numeric value of x. Values of "mate x" will be converted to the
#'   value specified by the `mate` parameter, in centipawns. The final scores
#'   will be returned as an integer vector.
#'
#' @note  The UCI convention is that scores for white are given with the
#'   opposite sign from the scores for black (*i.e.*, a positive score on
#'   black's turn means black is ahead, while a positive score on white's turn
#'   means white is ahead.) The function `convert_scores()` uses a different
#'   convention by default: the score for black will have the sign flipped to
#'   ensure that any time white is winning, score > 0, and any time black is
#'   winning score < 0. To override this behavior and use the UCI convention,
#'   set `flip_signs` to FALSE.
#'
#' @param scores A character vector of scores from a [UCI
#'   compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess engine.
#' @param mate (Default = 5000) A single-element integer vector of the value to
#'   use for 'mate x'.
#' @param flip_signs (Default = TRUE) A Boolean indicating whether to flip the
#'   sign of even numbered half-moves.
#'
#' @return An numeric vector of scores (in centipawns).
#' @export
#'
#' @seealso [rbitr::parse_gamelog()] to extract scores from engine output.
#'
#' @examples
#' scores <- c("90", "-26", "26 upperbound", "mate 1", "mate -1", "mate 0", NA)
#' convert_scores(scores)

convert_scores <- function(scores, mate = 5000, flip_signs = TRUE) {
  # Validate input
  stopifnot(is.character(scores))
  stopifnot(is.numeric(mate) & length(mate) == 1)
  stopifnot(is.logical(flip_signs))
  # Convert 'mate x' to numeric value
  mate0 <- which(scores == 'mate 0')
  if (length(mate0) > 0) {
    scores[mate0] <- -mate
  }
  scores[startsWith(scores, 'mate ') & !startsWith(scores, 'mate -')] <- mate
  scores[startsWith(scores, 'mate -')] <- -mate
  # Convert upperbound & lowerbound to numeric
  bounds <- grepl('bound', scores)
  if (any(bounds, na.rm = TRUE)) {
    scores[bounds] <- sub(' [a-z]*bound', '', scores[bounds])
  }
  # Remove cp markers
  scores <- sub('cp ', '', scores)
  # Convert characters to numeric. Note: as.integer(Inf) returns NA, so use
  # as.numeric() to preserve Inf values when mate == Inf.
  scores <- as.numeric(scores)
  # Flip signs for black scores so that if score > 0 white is winning.
  if (flip_signs) {
    black_index <- seq_along(scores) %% 2 == 0
    scores[black_index] <- -scores[black_index]
  }
  return(scores)
}



