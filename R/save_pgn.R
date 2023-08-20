#' Save chess game data in the PGN format
#'
#' A no-frills function to save a data frame of chess games in the portable game
#'   notation (PGN) format.
#'
#' @details The data frame (or tibble) to be saved as a PGN file should have a
#'   column for each desired tag. In addition, the table should have a column
#'   named Movetext containing the moves of each game in standard algebraic
#'   notation (SAN). The table should contain one game per row. It is currently
#'   up to the user to ensure that the movetext is valid SAN, since `save_pgn()`
#'   will do nothing to check the validity.
#'
#' @details The PGN format requires tag names to begin with an uppercase letter,
#'   and to consist only of letters, digits, or underscores.See the
#'   [PGN specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm)
#'   for information on the PGN format.
#'
#' @details If any tags in the
#'   [Seven Tag Roster](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1)
#'   are missing, `save_pgn()` will add the missing tags and assign them values
#'   of "?" to indicate that the values are unknown. Any forbidden characters in
#'   the tag names will be replaced with underscores. If this causes duplicate
#'   tags, the duplicates will be numbered. Tag names beginning with lowercase
#'   letters will be capitalized, and those beginning with non-letters will be
#'   prepended with a capital X.
#'
#' @param pgn A data frame or tibble of the games to be saved.
#' @param pgn_path A single-element character vector of the path where the PGN
#'   file should be saved.
#' @param append (Default = FALSE) A single-element boolean vector indicating
#'   whether to overwrite or append data to an already existing file.
#'
#' @return `save_pgn()` returns `NULL`.
#' @export
#'
#' @seealso
#'   * [rbitr::get_pgn()] for loading PGN files.
#'   * [rbitr::clean_movetext()] to strip comments and annotations.
#'   * [rbitr::evaluate_pgn()] to analyze all the games in a PGN file
#'   * [rbitr::evaluate_game()] to analyze a single game from a PGN file
#'
#' @examples
#' fools_mate <- tibble::tibble(
#'   Event    = 'Casual Game',
#'   Site     = '221B Baker Street',
#'   Date     = '1887.04.01',
#'   Round    = '?',
#'   White    = 'Sherlock Holmes',
#'   Black    = 'John H. Watson',
#'   Result   = '1-0',
#'   Movetext = '1. e4 g5 2. Nc3 f5 3. Qh5# 1-0'
#' )
#' save_pgn(fools_mate, pgn_path = 'save_pgn_example.pgn')
#' file.remove('save_pgn_example.pgn')
save_pgn <- function(pgn, pgn_path, append = FALSE) {
  # Validate input
  assertthat::assert_that(is.data.frame(pgn))
  assertthat::assert_that(assertthat::is.string(pgn_path))
  assertthat::assert_that(assertthat::is.flag(append))
  # Replace NAs with ?
  pgn <- as.data.frame(pgn)
  pgn <- replace(pgn, is.na(pgn), '?')
  # Replace any illegal characters with _, prepend leading digit or _ with X
  tag_names <- names(pgn)
  tag_names <- stringr::str_replace(tag_names, '\\W', '_')
  tag_names <- stringr::str_replace(tag_names, '(^)(\\d|_)', 'X\\2')
  # Number any tags that are now duplicates
  not_unique <- function(x) {
    duplicated(x) | duplicated(x, fromLast = TRUE)
  }
  duplicates <- tag_names[not_unique(tag_names)]
  if (length(duplicates) > 0) {
    duplicate_index <- 1:length(duplicates)
    tag_names[not_unique(tag_names)] <- paste0(duplicates, duplicate_index)
  }
  # Check seven tag roster, add any that are missing
  tag_names <- tag_names[tag_names != 'Movetext']
  seven_tag_roster <- c('Event', 'Site', 'Date', 'Round', 'White', 'Black',
                        'Result')
  missing_tag_names <- seven_tag_roster[!(seven_tag_roster %in% tag_names)]
  pgn[, missing_tag_names] <- rep('?', nrow(pgn))
  # Sort any extra tags and place at end
  extra_tags <- tag_names[!(tag_names %in% seven_tag_roster)]
  tag_names <- c(seven_tag_roster, extra_tags[order(extra_tags)])
  # Format the PGN text
  tagify <- function(tag_name, game) {
    tag_value <- game[1, tag_name]
    paste0('[', tag_name, ' "', tag_value, '"]', '\n')
  }
  game_indices <- 1:nrow(pgn)
  pgnify <- function(game_index, tag_names, pgn) {
    tag_pairs <- paste0(unlist(lapply(tag_names, tagify, pgn[game_index, ])),
                        collapse = '')
    paste0(tag_pairs, '\n', pgn$Movetext[game_index], '\n\n', collapse = '')
  }
  # Save the PGN
  sink(file = pgn_path, append = append)
  cat(paste0(unlist(lapply(game_indices, pgnify, tag_names, pgn)),
             collapse = ''))
  sink()
}
