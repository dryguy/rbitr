#' Get the tag names used in a pgn
#'
#' This is a helper function for `rbitr::read_pgn` to ensure that all available
#' tags  will be read from the file.
#'
#' @details See the pgn specification for the definition of tag pairs
#'   (http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1).
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
#' @family pgn file management tools
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

#' Read a pgn file
#'
#' Convenience wrapper specifying preferred rbitr defaults for
#' `bigchess::read.pgn`. The contents of the pgn file are read into a data
#' frame with  the tag names as column names. All tags in the pgn will be read.
#'
#' @param pgn_path A single-element character vector of the path to a pgn file.
#' @param verbose (Boolean, default = TRUE) Print a message showing the file
#'   being read. Useful to show progress when batch processing a lot of pgn
#'   files.
#'
#' @return A data frame of the pgn data (see `bigchess::read.pgn` for further
#'   details)
#' @export
#'
#' @family pgn file management tools
#'
#' @examples
#' pgn_path <- file.path(
#'   system.file(package = 'rbitr'),
#'   'extdata',
#'   'fools_mate.pgn'
#' )
#' read_pgn(pgn_path)
read_pgn <- function(pgn_path, verbose = FALSE) {
  if (verbose) {
    print(paste('Reading ', pgn_path, sep=''))
  }
  if (!file.exists(pgn_path)) {
    stop('File not found.')
  }
  existing_tags <- get_pgn_tags(pgn_path)
  default_tags <- c(
    'Event', 'Site', 'Date', 'Round', 'White', 'Black', 'Result',
    'Movetext', 'SourceMovetext', 'NMoves'
  )
  add.tags <- setdiff(existing_tags, default_tags)
  suppressWarnings(pgn <- bigchess::read.pgn(
    pgn_path,
    add.tags           = add.tags,
    n.moves            = TRUE,
    extract.moves      = 0,
    last.move          = FALSE,
    stat.moves         = FALSE,
    big.mode           = FALSE,
    quiet              = TRUE,
    ignore.other.games = TRUE,
    source.movetext    = TRUE
  ))
  # Result is the only column that defaults to a factor, which can be confusing
  pgn$Result <- as.character(pgn$Result)
  return(pgn)
}
