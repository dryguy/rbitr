#' Read a pgn file
#'
#' Convenience wrapper specifying preferred rbitr defaults for the
#' [bigchess](https://github.com/rosawojciech/bigchess) `read.pgn()` function.
#' The contents of the pgn file are read into a data frame with the tag names as
#' column names. All tags in the pgn will be read.
#'
#' @param pgn_path A single-element character vector of the path to a pgn file.
#' @param verbose (Boolean, default = TRUE) Print a message showing the file
#'   being read. Useful to show progress when batch processing a lot of pgn
#'   files.
#'
#' @return A data frame of the pgn data (see the
#' [bigchess](https://github.com/rosawojciech/bigchess) function `read.pgn()`
#' for further details)
#' @export
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
    print(paste('Reading ', pgn_path, sep = ''))
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
