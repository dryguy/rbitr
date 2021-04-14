#' Convert movetext to individual moves.
#'
#' Convert movetext in standard algebraic notation (SAN) to individual moves in
#'   long algebraic notation (LAN).
#'
#' @param movetext A character vector containing a sequence of moves in SAN.
#'
#' @return A recursive list of moves.
#' @export
#'
#' @examples
#' movetext <- '1. e4 g5 2. Nc3 f5 3. Qh5#'
#' get_moves(movetext)
get_moves <- function(movetext) {
  moves <- lapply(movetext, bigchess::san2lan)
  moves <- unlist(lapply(moves, tolower), use.names = FALSE)
  strsplit(moves, ' ', fixed = TRUE)
}
