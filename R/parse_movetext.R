#' Get command tags from pgn movetext
#'
#' Get the specified command tag (if present) from the movetext taken from a
#'   pgn file.
#'
#' @details See the
#'   [Portable Game Notation Specification and Implementation Guide (Supplement)](http://www.enpassant.dk/chess/palview/enhancedpgn.htm)
#'   for details on command tags in pgn files.
#'
#' @details The `get_clocks()` and `get_evals()` functions are intended for use
#'   with data read from a pgn using the `get_pgn()` function. A tibble obtained
#'   using `get_pgn()` will have a $Movetext column from which the tag data can
#'   be extracted if it is present.
#'
#' @param movetext A character vector of pgn movetext, where each vector entry
#'   is for a separate game.
#'
#' @return A list containing numeric vectors of the specified tag value. Each
#'   list entry is for a separate game. For `get_clocks()`, the value will be
#'   converted to seconds.
#' @export
#'
#' @examples
#' movetext <- c(
#'   '1. d4 {[%clk 0:15:00]} d5 {[%clk 0:15:00]} 2. Nc3 {[%clk 0:14:48]}',
#'   '1. e4 {[%clk 0:15:00]} e5 {[%clk 0:15:00]} 2. Nf3 {[%clk 0:14:38]}'
#' )
#' get_clocks(movetext)
#'
#' movetext <- c(
#'   '1. e4 { [%eval 0.05] } 1... Nf6 { [%eval 0.29] } 2. Bc4 { [%eval -0.94] }'
#' )
#' get_evals(movetext)
get_clocks <- function(movetext) {
  parse_movetext(movetext, 'clk')
}

#' @rdname get_clocks
#' @export
get_evals <- function(movetext) {
  parse_movetext(movetext, 'eval')
}

#' Parse movetext
#'
#' The function `parse_movetext()` is the parser behind `get_clocks()` and
#'   `get_evals()`.
#'
#' @param movetext A character vector of pgn movetext, where each vector entry
#'   is for a separate game.
#' @param cmd_name A single-element character vector of the command to parse
#'
#' @return A list containing numeric vectors of the specified tag value. Each
#'   list entry is for a separate game. For `get_clocks()`, the value will be
#'   converted to seconds.
parse_movetext <- function(movetext, cmd_name) {
  # Validate input
  assertthat::assert_that(is.character(movetext))
  assertthat::assert_that(cmd_name == 'clk' |
                          cmd_name == 'eval')

  # Parse the movetext
  cmd_regex <- paste0('\\[%', cmd_name, '\\s*([^]]*)')
  result <- lapply(
    stringr::str_match_all(movetext, cmd_regex), '[', , 2
  )
  if (cmd_name == 'clk') {
    result <- lapply(result, as.difftime, units = 'secs')
  }
  result <- lapply(result, as.numeric)
  if (cmd_name == 'eval') {
    result <- lapply(result, '*', 100)
  }
  result
}
