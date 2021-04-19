#' Get the tag names used in a large pgn.
#'
#' This is a helper function for rbitr's `read_pgn()` to ensure that all
#' available tags will be read from the file. It is an alternative to rbitr's
#' `get_pgn_tags()` intended for large pgn files. Unlike `get_pgn_tags`,
#' `get_big_pgn_tags()` only keeps a portion of the file in memory at a time,
#' and also periodically reports progress.
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
#' @param batch_size (Default = 10^7) A single-element integer vector indicating
#'   how many lines at a time are read from the file.
#' @param silent (Boolean, default = FALSE) Report progress?
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
#' get_big_pgn_tags(pgn_path)
get_big_pgn_tags <- function(pgn_path, batch_size = 10^7, silent = FALSE) {
  con <- file(pgn_path, "rt")
  batch_text <- readLines(con, batch_size)
  batch_length <- length(batch_text)
  n <- 0
  while (batch_length > 0) {
    if (exists('tag_names')) {
      # This part is skipped on the first batch.
      new_tag_names <- stringr::str_match(batch_text, '\\[\\s*([A-Z]\\w*)')[, 2]
      tag_names <- unique(c(tag_names, new_tag_names))
    } else {
      # This part only runs on the first batch.
      tag_names <- stringr::str_match(batch_text, '\\[\\s*([A-Z]\\w*)')[, 2]
      tag_names <- unique(tag_names)
    }
    batch_text <- readLines(con, batch_size)
    batch_length <- length(batch_text)
    n <- n + 1
    if (!silent) {
      message(paste(Sys.time(), "batch", n, "processed. Tag names so far:\n",
                    paste0(tag_names, collapse = ', ')))
    }
  }
  close(con)
  if (!silent) {
    message(paste0(Sys.time(), ", end of file."))
  }
  tag_names[!is.na(tag_names)]
}
