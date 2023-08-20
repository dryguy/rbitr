#' Remove comments and annotations from movetext
#'
#' Removes comments, annotations, and other formatting that would interfere with
#'   engine analysis.
#'
#' @details The [PGN specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.2)
#'   allows comments, annotations, game termination markers, and formatting
#'   oddities that may not play well with all software. The `clean_movetext()`
#'   function removes everything but the moves and move numbers, unless the
#'   `cut_gtm` parameter is set to `FALSE`.
#'
#' @note Game termination markers ('1-0', '0-1', '1/2-1/2', '*') are removed by
#'   default. This causes the movetext to no longer comply with the PGN
#'   specification. PGN files saved without game termination markers will not
#'   function properly. If you intend to save the clean movetext to a PGN file,
#'   set the `cut_gtm` parameter to `FALSE` to preserve the game termination
#'   markers.
#'
#' @note Contrary to the pgn specification, some sources may use zero for
#'   castling ('0-0' or '0-0-0')  in stead of capital letter O. Clean movetext
#'   will convert these to the accepted forms.
#'
#' @param movetext A character vector of PGN movetext data
#' @param cut_gtm (Default = TRUE) A boolean indicating if game termination
#'   markers should be deleted
#'
#' @return A character vector of clean movetext.
#' @export
#'
#' @seealso [rbitr::get_pgn()] to get the movetext and other data from a PGN
#'   file.
#'
#' @examples
#' clean_movetext('1. e4! {Best by test.} e5 (d5 leads to the Scandinavian.) *')

clean_movetext <- function(movetext, cut_gtm = TRUE) {
  assertthat::assert_that(is.character(movetext))
  assertthat::assert_that(assertthat::is.flag(cut_gtm))
  # Remove brace format comments
  movetext <- stringr::str_replace_all(movetext, '\\{[^}]*\\}', ' ')
  # Remove semicolon format comments
  movetext <- stringr::str_replace_all(movetext, ';[^\\\\]*[\n|\r]', ' ')
  # Remove line breaks
  movetext <- stringr::str_replace_all(movetext, '[\n|\r]', ' ')
  # Remove 1... style numbering
  movetext <- stringr::str_replace_all(movetext, '[0-9]+\\.\\.\\.', ' ')
  # Remove traditional annotations (!, !!, ?, ??, !?, ?!) and literal glyphs
  # Instead of NAGs, some websites use actual glyph characters which is not
  # compliant with the PGN specification. The actual glyph characters are not
  # used here as the code would be less portable. Instead the characters are
  # identified by their unicode codepoints.
  literal_glyph_regex <- paste0(
    '[!?]+| = | N |-\\+|\\+-',
    '|=/\u221e|\u221e/=|\u221e',
    '|\u2a72|\u2a71|\u00b1|\u2213',
    '|\u2a00|\u2192|\u2191|\u25a1',
    '|\u21c6|\u2295|\u2206'
  )
  movetext <- stringr::str_replace_all(movetext, literal_glyph_regex, ' ')
  # Remove numeric annotation glyphs (NAGs)
  movetext <- stringr::str_replace_all(movetext, '\\$[0-9]+', '')
  # Remove recursive variations
  movetext <- gsub('\\((?>[^()]|(?R))*\\)', ' ', movetext, perl = TRUE)
  # Remove extra spaces
  movetext <- stringr::str_replace_all(movetext, '\\s+', ' ')
  # Remove game termination markers
  if (cut_gtm) {
   movetext <- stringr::str_replace_all(movetext, '1-0|0-1|1\\/2-1\\/2|\\*', '')
  }
  # Replace zeros in castling symbols
  movetext <- gsub("0-0-0", "O-O-O", movetext)
  movetext <- gsub("0-0", "O-O", movetext)
  # Trim white space
  trimws(movetext)
}
