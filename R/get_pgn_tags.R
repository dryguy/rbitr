#' Get the tag names used in a pgn
#'
#' This is a helper function for rbitr's `read_pgn()` to ensure that all
#' available tags will be read from the file.
#'
#' @details See the
#' [pgn specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1)
#' for the definition of tag pairs.
#'
#' @details Every game in the pgn will be searched, and the name of every tag
#'   found will be reported, even if the tag only appears in only one game. If a
#'   tag appears more than once in the file, it will only appear once in the
#'   output; subsequent occurrences are ignored.
#'
#' @param pgn_path A single-element character vector of the path to a pgn file.
#'
#' @return A character vector of the unique tag names found in the pgn.
#' @export
#'
#' @examples
#' pgn_path <- file.path(
#'   system.file(package = "rbitr"),
#'   'extdata',
#'   'fools_mate.pgn'
#' )
#' get_pgn_tags(pgn_path)
get_pgn_tags <- function(pgn_path) {
  pgn_text  <- readLines(pgn_path)
  tag_names <- stringr::str_match(pgn_text, '\\[\\s*([A-Z]\\w*)')[, 2]
  tag_names <- unique(tag_names)
  tag_names[!is.na(tag_names)]
}
