#' Get the chess moves from a character vector containing movetext
#'
#' Given a character vector of movetext, return a list of character vectors
#'   each containing moves in long algebraic notation (LAN).
#'
#' @details The movetext field of a pgn record contains chess moves in standard
#'   algebraic notation, along with optional annotations and comments. The
#'   `get_moves()` function extracts the individual moves and converts them to
#'   long algebraic notation (LAN). The movetext vectors are typically obtained
#'   from a pgn file using the `get_pgn()` function.
#'
#' @param movetext A character vector of movetext.
#'
#' @return A list of character vectors, where each vector contains the LAN
#'   format moves of the corresponding movetext.
#' @export
#'
#' @seealso
#'   * [rbitr::get_pgn()] to get the movetext and other data from a pgn file.
#'   * [rbitr::clean_movetext()] to remove comments and annotations from
#'     movetext.
#'
#' @examples
#' movetext <- c('1. e4 {Best by test.} e5', '1. h3?! h6')
#' get_moves(movetext)

get_moves <- function(movetext) {
  assertthat::assert_that(is.character(movetext))
  movetext <- clean_movetext(movetext)
  moves <- lapply(movetext, bigchess::san2lan)
  moves <- unlist(lapply(moves, tolower), use.names = FALSE)
  strsplit(moves, ' ', fixed = TRUE)
}
