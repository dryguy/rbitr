get_pgn_tags <- function (pgn_path) {
  # Retrieve all of the tag names in the specified pgn file.
  #
  # See the pgn specification for the definition of tag pairs:
  # (http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1):
  #
  # Arguments:
  #   pgn_path: A single-element character vector of the path to a pgn file
  #
  # Returns:
  #   A character vector of the tag names found in the pgn

  # Use regex to find '[' followed by optional spaces, followed by an uppercase
  # letter, followed by any letters, digits or underscores.

  pgn_text  <- readLines(pgn_path)
  tag_names <- stringr::str_match(pgn_text, '\\[\\s*([A-Z]\\w*)')[, 2]
  tag_names <- unique(tag_names)
  tag_names[!is.na(tag_names)]
}

read_pgn <- function (
  pgn_path,
  add.tags           = NULL, # This input is ignored. Hard coded to get all.
  n.moves            = TRUE,
  extract.moves      = 0,
  last.move          = FALSE,
  stat.moves         = FALSE,
  big.mode           = FALSE,
  quiet              = TRUE,
  ignore.other.games = TRUE,
  source.movetext    = TRUE,
  verbose            = TRUE
) {
  # Convenience wrapper specifying preferred rbitr defaults for
  # bigchess::read.pgn. All tags in the pgn will be read.
  #
  # Arguments:
  #   pgn_path: A single-element character vector of the path to a pgn file
  #
  # Returns:
  #   A data frame of the pgn data (see bigchess::read.pgn for further details)

  if (verbose) {
    print(paste('Reading ', pgn_path, sep=''))
  }
  if (!file.exists(pgn_path)) {
    stop('File not found.')
    return()
  }
  existing_tags <- get_pgn_tags(pgn_path)
  default_tags <- c(
    'Event', 'Site', 'Date', 'Round', 'White', 'Black', 'Result',
    'Movetext', 'SourceMovetext', 'NMoves'
  )
  add.tags <- setdiff(existing_tags, default_tags)
  suppressWarnings(pgn <- read.pgn(
    pgn_path,
    add.tags           = add.tags,
    n.moves            = n.moves,
    extract.moves      = extract.moves,
    last.move          = last.move,
    stat.moves         = stat.moves,
    big.mode           = big.mode,
    quiet              = quiet,
    ignore.other.games = ignore.other.games,
    source.movetext    = source.movetext
  ))
  # Result reads in as a factor, which is annoying
  pgn$Result <- as.character(pgn$Result)
  return(pgn)
}
