#' Get the contents of a PGN file
#'
#' A no-frills function to read a PGN file into a tibble.
#'
#' @details See the
#' [PGN specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete)
#' for information on the PGN format.
#'
#' @details Each distinct tag name in the PGN file will become a tibble column
#'   with the same name as the tag. The movetext field will be an additional
#'   column with the name Movetext.
#'
#' @param pgn_path A single-element character vector of the path to a PGN file.
#'
#' @return A tibble where each row is a game, and each PGN tag is a column, and
#'   the PGN file's movetext field is also a column.
#' @export
#'
#' @seealso
#'   * [rbitr::evaluate_pgn()] to analyze all the games in a PGN file
#'   * [rbitr::evaluate_game()] to analyze a single game from a PGN file
#'   * [rbitr::clean_movetext()] to strip comments and annotations.
#'   * [rbitr::get_clocks()] to extract clock data.
#'   * [rbitr::get_evals()] to extract evaluations.
#'   * [rbitr::get_increments()] to extract increments.
#'
#' @examples
#' pgn_path <- file.path(
#'   system.file(package = 'rbitr'),
#'   'extdata',
#'   'fools_mate.pgn'
#' )
#' get_pgn(pgn_path)
get_pgn <- function(pgn_path) {
  assertthat::assert_that(assertthat::is.string(pgn_path))
  # Start with 0 games
  pgn <- data.frame()
  # Open the file
  con <- file(pgn_path, 'rt')
  # Initialize flags
  end_of_file <- FALSE
  end_of_game <- FALSE
  newline <- ''
  # Make sure to check for end of game/file every time readLines is called
  get_newline <- function(con) {
    # Omit blank lines and escaped lines (% = escape)
    newline <<- readLines(con, 1, warn = FALSE, encoding = 'UTF-8')
    if (length(newline) == 0) {
      end_of_file <<- TRUE
      return()
    }
    while (newline == '') {
      newline <<- readLines(con, 1, warn = FALSE, encoding = 'UTF-8')
      if (length(newline) == 0) {
        end_of_file <<- TRUE
        return()
      }
    }
    while (stringr::str_sub(newline, 1, 1) == '%') {
      newline <<- readLines(con, 1, warn = FALSE, encoding = 'UTF-8')
      if (length(newline) == 0) {
        end_of_file <<- TRUE
        return()
      }
    }
    # Check for end of game
    termination_markers <- c('1-0', '0-1', '1/2-1/2', '*')
    if (stringr::str_sub(newline, 1, 1) != '[') {
      if (any(stringr::str_detect(newline,
                                  stringr::fixed(termination_markers)))) {
        end_of_game <<- TRUE
        return()
      }
    }
  }
  # Define the regex pattern for PGN tags: [TagName "TagValue"]
  tag_regex <- r'(\[\s*([A-Z][\w]*)\s*"([^"]*)"\s*\])'
  get_newline(con)
  while (!end_of_file) {
    # Read in the next tag section
    tag_section <- character(0)
    while (stringr::str_sub(newline, 1, 1) == '[' & !end_of_file) {
      tag_section <- c(tag_section, newline)
      get_newline(con)
      if (length(newline) == 0) {
        break
      }
    }
    # Read in the next movetext section
    movetext_section <- character(0)
    while (!end_of_game & !end_of_file) {
      movetext_section <- paste(movetext_section, newline, '\n')
      get_newline(con)
    }
    movetext_section <- paste(movetext_section, newline)
    # Add the game to the pgn data frame
    parsed_tags <- stringr::str_match_all(tag_section, tag_regex)
    parsed_tags <- do.call(rbind, parsed_tags)
    tag_values <- as.list(parsed_tags[, 3])
    tag_names <- as.list(parsed_tags[, 2])
    current_game <- as.data.frame(tag_values, col.names = tag_names)
    if (nrow(current_game) > 0) {
      if (length(movetext_section) > 0) {
        current_game$Movetext <- trimws(movetext_section)
      } else {
        current_game$Movetext <- NA
      }
    }
    pgn <- dplyr::bind_rows(pgn, current_game)
    end_of_game <- FALSE
    get_newline(con)
  }
  # Close the file & return the result as a tibble for nicer printing
  close(con)
  tibble::as_tibble(pgn)
}
