#' Remove comments and annotations from movetext.
#'
#' The [pgn specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.2)
#' allows comments, annotations, game termination markers, and other formatting
#' oddities that may not play well with all software. The `clean_movetext()`
#' function removes everything but the moves and move numbers.
#'
#' @param movetext A character vector of pgn movetext data
#'
#' @return A character vector of clean movetext.
#' @export
#'
#' @seealso [rbitr::get_pgn()] to get the movetext and other data from a pgn
#'   file.
#'
#' @examples
#' clean_movetext('1. e4! {Best by test.} e5 (d5 leads to the Scandinavian.) *')

clean_movetext <- function(movetext) {
  assertthat::assert_that(is.character(movetext))
  # Remove semicolon format comments
  movetext <- stringr::str_replace_all(movetext, ';[^\\\\]*[\n|\r]', ' ')
  # Remove line breaks
  movetext <- stringr::str_replace_all(movetext, '[\n|\r]', ' ')
  # Remove brace format comments
  movetext <- stringr::str_replace_all(movetext, '\\{[^}]*\\}', ' ')
  # Remove 1... style numbering
  movetext <- stringr::str_replace_all(movetext, '[0-9]+\\.\\.\\.', ' ')
  # Remove traditional annotations (!, !!, ?, ??, !?, ?!)
  movetext <- stringr::str_replace_all(movetext, '[?|!]', '')
  # Remove numeric annotation glyphs (NAGs)
  movetext <- stringr::str_replace_all(movetext, '\\$[0-9]+', '')
  # Remove recursive variations
  movetext <- gsub('\\((?>[^()]|(?R))*\\)', ' ', movetext, perl = TRUE)
  # Remove extra spaces
  movetext <- stringr::str_replace_all(movetext, '\\s+', ' ')
  # Remove game termination markers
  movetext <- stringr::str_replace_all(movetext, '1-0|0-1|1\\/2-1\\/2|\\*', '')
  # Trim white space
  trimws(movetext)
}
