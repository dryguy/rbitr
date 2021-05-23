#' Get command tags from pgn movetext
#'
#' Get the specified command tag (if present) from the movetext taken from a
#'   pgn file.
#'
#' @details See the
#'   [Portable Game Notation Specification and Implementation Guide (Supplement)](http://www.enpassant.dk/chess/palview/enhancedpgn.htm)
#'   for details on command tags in pgn files.
#'
#' @details The `get_clocks()` and `get_evals()` functions are intended for use
#'   with data read from a pgn using the `get_pgn()` function. A tibble obtained
#'   using `get_pgn()` will have a $Movetext column from which the tag data can
#'   be extracted if it is present.
#'
#' @note For games stored in pgn format, the evaluation of the first position
#'   before any moves are made is not commonly included. As a result,
#'   `get_evals()` produces a vector that is one evaluation shorter than the
#'   one produced by `parse_gamelog()` for the same game. To include a value for
#'   the initial position, set the parameter first_eval to the desired value.
#'   To use the same default as [lichess.org](http://lichess.org), set
#'   first_eval to 15.
#'
#' @note For games ending in mate, a uci engine gives the final position an
#'   evaluation of 'mate 0', however, in a pgn, any '#x' notation is omitted for
#'   the final position when it is mate. When the final position is mate,
#'   [lichess.org](http://lichess.org) advantage plots show an advantage of 1
#'   for the final position, but the value is not used in calculating acpl. To
#'   include an evaluation for mated positions, set the mate0 parameter to TRUE.
#'
#'
#' @param movetext A character vector of pgn movetext, where each vector entry
#'   is for a separate game.
#' @param first_eval (Default = NULL) A single-element integer vector indicating
#'   what value should be assigned to the initial position, before white's first
#'   move. The default (NULL) omits the first evaluation.
#' @param mate0 (Default = FALSE) A single-element boolean vector indicating
#'   whether to include a value for mated positions.
#'
#' @return A list containing numeric vectors of the specified tag value. Each
#'   list entry is for a separate game. For `get_clocks()`, the value will be
#'   converted to seconds.
#' @export
#'
#' @examples
#' movetext <- c(
#'   '1. d4 {[%clk 0:15:00]} d5 {[%clk 0:15:00]} 2. Nc3 {[%clk 0:14:48]}',
#'   '1. e4 {[%clk 0:15:00]} e5 {[%clk 0:15:00]} 2. Nf3 {[%clk 0:14:38]}'
#' )
#' get_clocks(movetext)
#'
#' movetext <- c(
#'   '1. e4 { [%eval 0.05] } 1... Nf6 { [%eval 0.29] } 2. Bc4 { [%eval -0.94] }'
#' )
#' get_evals(movetext)
get_clocks <- function(movetext) {
  parse_movetext(movetext, cmd_name = 'clk', first_eval = NULL,
                 mate0 = NULL, mate_value = 35000)
}

#' @rdname get_clocks
#' @export
get_evals <- function(movetext, first_eval = NULL, mate0 = FALSE,
                      mate_value = 35000) {
  parse_movetext(movetext, cmd_name = 'eval',
                 first_eval = first_eval, mate0 = mate0,
                 mate_value = mate_value)
}

#' Parse movetext
#'
#' The function `parse_movetext()` is the parser behind `get_clocks()` and
#'   `get_evals()`.
#'
#' @param movetext A character vector of pgn movetext, where each vector entry
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
                           mate_value = 35000) {
  # Validate input
  assertthat::assert_that(is.character(movetext))
  assertthat::assert_that(cmd_name == 'clk' |
                          cmd_name == 'eval')
  assertthat::assert_that(is.integer(first_eval) |
                          is.numeric(first_eval) |
                          is.null(first_eval))
  assertthat::assert_that(assertthat::is.flag(mate0) |
                          is.null(mate0))
  assertthat::assert_that(is.numeric(mate_value) | is.integer(mate_value))
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
