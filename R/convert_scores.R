#' Convert parsed score values to numeric scores.
#'
#' @details The scores obtained from parsing the output of a
#' [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#' engine are a mix of centipawn scores, mate in x, and/or bounds on the score.
#' In addition, the scores for white will have opposite sign from the scores for
#' black. The function `convert_scores()` will convert bounds or mate in x into
#' numeric values. It will also change the sign of black's scores, so that
#' positive evaluations will mean white is ahead, and negative will mean black
#' is ahead.
#'
#' @param scores A character vector of scores from a
#'   [UCI compatible](http://wbec-ridderkerk.nl/html/UCIProtocol.html) chess
#'   engine.
#'
#' @return An integer vector of scores (in centipawns).
#' @export
#'
#' @examples
#' scores <- c("90", "-26", "26 upperbound", "mate -2" , "mate 1", "mate -1", "mate 0", NA)
#' convert_scores(scores)
convert_scores <- function(scores) {
  # Validate input
  assertthat::assert_that(is.character(scores))
  # Convert 'mate x' to numeric value
  ply <- 1:length(scores)
  mate0 <- which(stringr::str_detect(scores, 'mate 0'))
  if (ply[mate0] %% 2 == 1) {
    scores[mate0] <- 5000
  }
  if (ply[mate0] %% 2 == 0) {
    scores[mate0] <- -5000
  }
  scores[stringr::str_detect(scores, 'mate [0-9]+')] <- 5000
  scores[stringr::str_detect(scores, 'mate -[0-9]+')] <- -5000
  # Convert upperbound & lowerbound to numeric
  bounds <- stringr::str_detect(scores, 'bound')
  if (any(bounds, na.rm = TRUE)) {
    scores[which(bounds)] <- stringr::str_replace(scores[which(bounds)],
                                                  ' [a-z]*bound', '')
  }
  # Convert characters to integer
  scores <- as.integer(scores)
  # Convert signs so that if score > 0 white is winning.
  index <- 1:length(scores)
  scores[index %% 2 == 0] <- -scores[index %% 2 == 0]
  scores
}
