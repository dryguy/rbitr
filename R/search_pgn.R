#' Search a pgn for games meeting the search criterion.
#'#'
#' @details See the
#' [pgn specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1)
#' for information on the pgn format.
#'
#' @details The pgn will be searched for the specified text, and if found,
#' games containing the search text will be returned.
#'
#' @param pgn_path A single-element character vector of the path to a pgn file.
#' @param pattern A single-element character vector containing the text to
#'   search for.
#' @param n_max (Default = -1) A single-element integer vector of the maximum
#'   number of matches to return. The default value of -1 returns all matches.
#'
#' @return A data frame of the games found.
#' @export
#'
#' @examples
#' pgn_path <- file.path(
#'   system.file(package = "rbitr"),
#'   'extdata',
#'   'fools_mate.pgn'
#' )
#' search_pgn(pgn_path, '1. e4 e5')
search_pgn <- function(pgn_path, pattern, n_max = -1) {
  assertthat::assert_that(assertthat::is.string(pgn_path))
  assertthat::assert_that(assertthat::is.string(pattern))
  assertthat::assert_that(assertthat::is.count(n_max) | n_max == -1)
  # Start with 0 games
  pgn <- data.frame()
  # Open the file
  con <- file(pgn_path, 'rt')
  # Initialize flags
  end_of_file <- FALSE
  end_of_game <- FALSE
  pattern_found <- FALSE
  n_matches <- 0
  newline <- ''
  # Make sure to check for end of game/file every time readLines is called.
  get_newline <- function(con) {
    # Omit blank lines
    newline <<- readLines(con, 1)
    if (length(newline) == 0) {
      end_of_file <<- TRUE
      return()
    }
    while (newline == '') {
      newline <<- readLines(con, 1)
    }
    if (length(newline) == 0) {
      end_of_file <<- TRUE
      return()
    }
    termination_markers <- c('1-0', '0-1', '1/2-1/2', '*')
    if (stringr::str_sub(newline, 1, 1) != '[') {
      if (any(stringr::str_detect(newline,
                                  stringr::fixed(termination_markers)))) {
        end_of_game <<- TRUE
        return()
      }
    }
  }
  # Define the regex pattern for pgn tags: [TagName "TagValue"]
  tag_regex <- r'(\[\s*([A-Z][\w]*)\s*"([^"]*)"\s*\])'
  get_newline(con)
  while (!end_of_file) {
    # Read in the next tag section
    tag_section <- character(0)
    while (stringr::str_sub(newline, 1, 1) == '[' & !end_of_file) {
      tag_section <- c(tag_section, newline)
      get_newline(con)
    }
    # Read in the next movetext section
    movetext_section <- character(0)
    while (!end_of_game & !end_of_file) {
      movetext_section <- paste(movetext_section, newline)
      get_newline(con)
    }
    movetext_section <- paste(movetext_section, newline)
    # Search the game for a pattern match.
    if (any(stringr::str_detect(tag_section, stringr::fixed(pattern)))) {
      pattern_found <- TRUE
    }
    if (stringr::str_detect(movetext_section, stringr::fixed(pattern))) {
      pattern_found <- TRUE
    }
    # Also check for pattern after comments are removed
    parsed_movetext <- stringr::str_replace_all(movetext_section, '\\{[^}]*\\}', '')
    parsed_movetext <- stringr::str_replace_all(parsed_movetext, '[0-9]+\\.\\.\\.', '')
    parsed_movetext <- stringr::str_replace_all(parsed_movetext, '\\s+', ' ')
    if (stringr::str_detect(parsed_movetext, stringr::fixed(pattern))) {
      pattern_found <- TRUE
    }
    # Add the game to the pgn data frame
    if (pattern_found) {
      n_matches <- n_matches + 1
      parsed_tags <- stringr::str_match_all(tag_section, tag_regex)
      parsed_tags <- do.call(rbind, parsed_tags)
      tag_values <- as.list(parsed_tags[, 3])
      tag_names <- as.list(parsed_tags[, 2])
      current_game <- as.data.frame(tag_values, col.names = tag_names)
      current_game$Movetext <- trimws(movetext_section)
      pgn <- dplyr::bind_rows(pgn, current_game)
      print(pgn)
      pattern_found <- FALSE
      if (n_matches == n_max) {
        close(con)
        return(pgn)
      }
    }
    end_of_game <- FALSE
    get_newline(con)
  }
  # Close the file & return the result
  close(con)
  pgn
}
