#' Get clock data from pgn movetext
#'
#' Get the clock data (if present) from the movetext taken from a pgn file.
#'
#' @details See the
#'   [Portable Game Notation Specification and Implementation Guide (Supplement)](http://www.enpassant.dk/chess/palview/enhancedpgn.htm)
#'   for details on the clk command in pgn files.
#'
#' @details The `get_clocks()` function is intended for use with data read from
#'   a pgn using the `get_pgn()` function. A tibble obtained using `get_pgn()`
#'   will have a $Movetext column from which the clock data can be extracted if
#'   it is present.
#'
#' @param movetext A character vector of pgn movetext, where each vector entry
#'   is for a separate game.
#'
#' @return A list containing numeric vectors of time remaining after each move,
#'   in seconds. Each list entry is for a separate game.
#' @export
#'
#' @examples
#' movetext <- c(
#'   '1. d4 {[%clk 0:15:00]} d5 {[%clk 0:15:00]} 2. Nc3 {[%clk 0:14:48]}',
#'   '1. e4 {[%clk 0:15:00]} e5 {[%clk 0:15:00]} 2. Nf3 {[%clk 0:14:38]}'
#' )
#' get_clocks(movetext)
get_clocks <- function(movetext) {
  assertthat::assert_that(is.character(movetext))
  clock <- lapply(
    stringr::str_match_all(movetext, '\\[%clk\\s*([^]]*)'), '[', , 2
  )
  clock <- lapply(clock, as.difftime, units = 'secs')
  lapply(clock, as.numeric)
}
