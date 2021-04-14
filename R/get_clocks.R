#' Get clock data from pgn movetext
#'
#' Extract clock data (if present) from the movetext of a pgn file.
#'
#' @details See the
#'   [Portable Game Notation Specification and Implementation Guide (Supplement)](http://www.enpassant.dk/chess/palview/enhancedpgn.htm)
#'   for details on the clk command in pgn files.
#'
#' @note For pgns read with the bigchess function `read.pgn()` or the rbitr
#'   function `read_pgn()`, the $Movetext column is stripped of embedded
#'   commands and will not contain clock data. The original, unmodified movetext
#'   is found in the $SourceMovetext column.
#'
#' @param source_movetext A character vector of pgn movetext.
#'
#' @return A list containing numeric vectors of time remaining after each move,
#' in seconds.
#' @export
#'
#' @examples
#' source_movetext <- c(
#'   '1. d4 {[%clk 0:15:00]} d5 {[%clk 0:15:00]} 2. Nc3 {[%clk 0:14:48]}',
#'   '1. e4 {[%clk 0:15:00]} e5 {[%clk 0:15:00]} 2. Nf3 {[%clk 0:14:38]}'
#' )
#' get_clocks(source_movetext)
get_clocks <- function(source_movetext) {
  clock <- lapply(
    stringr::str_match_all(source_movetext, '\\[%clk\\s*([^]]*)'), '[', , 2
  )
  clock <- lapply(clock, as.difftime, units = 'secs')
  lapply(clock, as.numeric)
}
