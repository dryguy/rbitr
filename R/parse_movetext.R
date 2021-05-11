#' Get command tags from pgn movetext
#'
#' Get the specified command tag (if present) from the movetext taken from a
#'   pgn file.
#'
#' @details See the
#'   [Portable Game Notation Specification and Implementation Guide (Supplement)](http://www.enpassant.dk/chess/palview/enhancedpgn.htm)
#'   for details on command tags in pgn files.
#'
#' @details The `parse_movetext()` function is intended for use with data read
#'   from a pgn using the `get_pgn()` function. A tibble obtained using
#'   `get_pgn()` will have a $Movetext column from which the tag data can be
#'   extracted if it is present.
#'
#' @param movetext A character vector of pgn movetext, where each vector entry
#'   is for a separate game.
#' @param cmd_name A single-element character vector of the pgn command to parse.
#'
#' @return A list containing numeric vectors of the specified tag value. Each
#'   list entry is for a separate game. For the clk tag, the value will be
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
  lapply(result, as.numeric)
}
