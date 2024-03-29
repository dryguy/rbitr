#' Extract data from movetext
#'
#' Extract data from the from the movetext taken from a PGN file.
#'
#' @details See the
#'   [Portable Game Notation Specification and Implementation Guide (Supplement)](http://www.enpassant.dk/chess/palview/enhancedpgn.htm)
#'   for details on command tags in PGN files.
#'
#' @details The `get_clocks()` and `get_evals()` functions are intended for use
#'   with data read from a PGN file using the `get_pgn()` function. A tibble
#'   obtained using `get_pgn()` will have a $Movetext column from which the tag
#'   data can be extracted if it is present.
#'
#' @note For games stored in PGN format, the evaluation of the first position
#'   before any moves are made is not commonly included. As a result,
#'   `get_evals()` produces a vector that is  shorter than the one produced by
#'   `parse_gamelog()` for the same game. The `first_eval` parameter can be used
#'   to provide a value for this missing evaluation, if desired.
#'
#' @note For games ending in mate, a UCI engine gives the final position an
#'   evaluation of 'mate 0', however, in a PGN file, '#0' is omitted when the
#'   final position is mate. To include an evaluation for mated final positions,
#'   set the mate0 parameter to TRUE.
#'
#'
#' @param movetext A character vector of movetext.
#' @param first_eval (Default = NULL) A single-element integer vector indicating
#'   what value should be assigned to the initial position, before white's first
#'   move. The default (NULL) adds no initial value.
#' @param mate0 (Default = FALSE) A single-element boolean vector indicating
#'   whether to include a value when the final position is mate.
#' @param mate_value (Default = 50000) A single-element numeric vector of the
#'   centipawn value to assign for mate.
#'
#' @return A list containing numeric vectors of the specified tag value. Each
#'   list entry is for a separate game. For `get_clocks()`, the value will be
#'   converted to seconds.
#' @export
#'
#' @seealso
#'   * [rbitr::get_pgn()] to load a PGN file.
#'   * [rbitr::get_increments()] to get increment times.
#'   * [rbitr::get_move_times()] to calculate move times.
#'   * [rbitr::advantage_plot()] to plot advantage data.
#'   * [rbitr::time_plot()] to plot move time data.
#'
#' @examples
#' movetext <- c(
#'   '1. d4 {[%clk 0:15:00]} d5 {[%clk 0:15:00]} 2. Nc3 {[%clk 0:14:48]}',
#'   '1. e4 {[%clk 0:15:00]} e5 {[%clk 0:15:00]} 2. Nf3 {[%clk 0:14:38]}'
#' )
#' get_clocks(movetext)
#'
#' movetext <- c(
#'   '1. e4 {[%eval 0.05]} 1... Nf6 {[%eval 0.29]} 2. Bc4 {[%eval -0.94]}'
#' )
#' get_evals(movetext)
get_clocks <- function(movetext) {
  parse_movetext(movetext, cmd_name = 'clk', first_eval = NULL,
                 mate0 = NULL, mate_value = 50000)
}

#' @rdname get_clocks
#' @export
get_evals <- function(movetext, first_eval = NULL, mate0 = FALSE,
                      mate_value = 50000) {
  parse_movetext(movetext, cmd_name = 'eval',
                 first_eval = first_eval, mate0 = mate0,
                 mate_value = mate_value)
}

#' Parse movetext
#'
#' The function `parse_movetext()` is the parser behind `get_clocks()` and
#'   `get_evals()`.
#'
#' @param movetext A character vector of movetext, where each vector entry
#'   is for a separate game.
#' @param cmd_name A single-element character vector of the command to parse
#' @param first_eval (Default = NULL) A single-element integer vector indicating
#'   what value should be assigned to the initial position, before white's first
#'   move. The default (NULL) omits the first evaluation.
#' @param mate_value (Default = 35000) The value (in centipawns) to assign to
#'   positions evaluated as 'mate in x'.
#' @param mate0 (Default = FALSE) A single-element boolean vector indicating
#'   whether to include a value for mated positions ('mate in 0').
#'
#' @return A list containing numeric vectors of the specified tag value. Each
#'   list entry is for a separate game. For `get_clocks()`, the value will be
#'   converted to seconds.
parse_movetext <- function(movetext, cmd_name, first_eval = NULL, mate0 = FALSE,
                           mate_value = 50000) {
  # Validate input
  assertthat::assert_that(is.character(movetext))
  assertthat::assert_that(cmd_name == 'clk' |
                          cmd_name == 'eval')
  assertthat::assert_that(is.numeric(first_eval) |
                          is.null(first_eval))
  assertthat::assert_that(assertthat::is.flag(mate0) |
                          is.null(mate0))
  assertthat::assert_that(is.numeric(mate_value))
  # Parse the movetext
  cmd_regex <- paste0('\\[%', cmd_name, '\\s*([^]]*)')
  result <- lapply(
    stringr::str_match_all(movetext, cmd_regex), '[', , 2
  )
  if (cmd_name == 'clk') {
    result <- lapply(result, as.difftime, units = 'secs')
  }
  if (cmd_name == 'eval') {
    result <- lapply(result, stringr::str_replace_all, '#-\\d+',
                     as.character(-mate_value / 100))
    result <- lapply(result, stringr::str_replace_all, '#\\d+',
                     as.character(mate_value / 100))
    result <- lapply(result, as.numeric)
    result <- lapply(result, '*', 100)
    if (mate0) {
      cleaned_movetext <- clean_movetext(movetext)
      white_regex <- '\\d+\\.\\s\\w+\\+?#'
      black_regex <- '\\d+\\.\\s\\w+\\+?\\s\\w+#'
      is_mate <- function(cleaned_movetext, pattern) {
        stringr::str_detect(cleaned_movetext, pattern)
      }
      white_mate_index <- which(unlist(lapply(cleaned_movetext, is_mate, white_regex)))
      black_mate_index <- which(unlist(lapply(cleaned_movetext, is_mate, black_regex)))
      add_mate <- function(result, mate_value) {
        c(result, mate_value)
      }
      result[white_mate_index] <- lapply(result[white_mate_index], add_mate, mate_value)
      result[black_mate_index] <- lapply(result[black_mate_index], add_mate, -mate_value)
    }
  }
  if (!is.null(first_eval)) {
    add_first_eval <- function(result, first_eval) {
      c(first_eval, result)
    }
    result <- lapply(result, add_first_eval, first_eval)
  }
  lapply(result, as.numeric)
}
