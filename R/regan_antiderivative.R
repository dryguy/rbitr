#' Regan Antiderivative
#'
#' Calculate the antiderivative of 1 / (1 + abs(x)).
#'
#' @details This is a convenience function for calculating the definite
#'   integral:
#'
#'   di = integral from v0 = eval(m0) to vi = eval(mi) of 1 / (1 + abs(x)) dx
#'
#'   used by Regan, et al., to scale the difference in the evaluation of an
#'   engine's "best" move (m0) and a player's actual move (mi). The scaling
#'   compensates for the human tendency to evaluate on a log scale.
#'
#' @details The antiderivative of 1 / (1 + abs(x)) is:
#'
#' 1/2 * (sign(x) * (log(1 - x) + log(x + 1)) - log(1 - x) + log(x + 1))
#'
#' @note The antiderivative is undefined at 1 or -1. At these points the
#'   function will return the corresponding limit (+-log(4)/2).
#'
#' @param x An numeric vector of chess evaluations, in centipawns.
#'
#' @return A complex vector of the antiderivative of 1 / (1 + abs(x)).
#' @export
#'
#' @references
#' * Regan, K. W., & Haworth, G. M. C. (2011). Intrinsic Chess Ratings.
#'     Twenty-Fifth AAAI Conference on Artificial Intelligence, 834–839.
#'     https://www.aaai.org/ocs/index.php/AAAI/AAAI11/paper/view/3779
#' * Regan, K. W., Macieja, B., & Haworth, G. M. C. (2012).
#'     Understanding distributions of chess performances. Lecture Notes in
#'     Computer Science, 7168, 230–243.
#'     https://doi.org/10.1007/978-3-642-31866-5_20
#'
#' @examples
#' regan_antiderivative(c(-100, -10, -1, 0, 1, 10, 100))
regan_antiderivative <- function(x) {
  assertthat::assert_that(is.numeric(x))
  y <- as.complex(x)
  result <- 1/2 * (
    sign(x)
    * (log(1 - y) + log(y + 1))
    -  log(1 - y) + log(y + 1)
  )
  result[x == -1] <- -log(4)/2  # Function is undefined at -1, 1
  result[x ==  1] <-  log(4)/2  # so assign limiting values
  return(Re(result))
}
