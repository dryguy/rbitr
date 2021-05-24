#' Gets LAN format moves from a pgn movetext vector
#'
#' Given a character vector of movetext (typically obtained from a pgn using
#'   rbitr's `get_pgn()` function), return a list of character vectors of LAN
#'   format moves.
#'
#' @param movetext A character vector of one or more movetext fields from a pgn.
#'
#' @return A list of character vectors, where each vector contains the LAN
#'   format moves of the corresponding movetext.
#' @export
#'
#' @examples
#' movetext <- c('1. e4 e5', '1. h3 h6')
#' get_moves(movetext)
get_moves <- function(movetext) {
  assertthat::assert_that(is.character(movetext))
  movetext <- clean_movetext(movetext)
  moves <- lapply(movetext, bigchess::san2lan)
  moves <- unlist(lapply(moves, tolower), use.names = FALSE)
  strsplit(moves, ' ', fixed = TRUE)
}
