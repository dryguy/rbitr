#' Extract data from the output of rbitr's `evaluate_game()` function.
#'
#' The output of rbitr's `evaluate_game()` function is a list, where each entry
#'   is the engine output for a single position. The `parse_gamelog()` function
#'   can extract the positional evaluations in centipawns (score), principal
#'   variations (pv), or best moves (bestmove).
#'
#' @details The data for the deepest available search depth is returned by
#' default. See
#' [the UCI protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html) for
#' details of UCI engine output.
#'
#' @param gamelog A list of engine output from rbitr's `evaluate_game()`
#'   function.
#' @param target A single-element character vector of the output to return.
#'   Allowed values are 'score' for the evaluation in centipawns, 'pv' for the
#'   principal variation, or 'bestmove' for the best move.
#' @param depth (Optional, default = NULL) A single-element integer vector
#'   indicating which search depth to return. The value must not be less than 1
#'   or greater than the maximum depth reported by the engine. A value of NULL
#'   returns data for the maximum depth.
#'
#' @return A list of character vectors of the extracted data, where each list
#'   entry corresponds to a position.
#' @export
#'
#' @examples
#' gamelog <- list(c(
#'   "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
#'   "readyok",
#'   "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
#'   "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
#'   "bestmove d2d4"
#' ))
#' parse_gamelog(gamelog, 'score')
#' parse_gamelog(gamelog, 'pv')
#' parse_gamelog(gamelog, 'bestmove')
parse_gamelog <- function(gamelog, target, depth = NULL) {
  # Validate input
  assertthat::assert_that(is.list(gamelog))
  assertthat::assert_that(assertthat::is.count(depth) | is.null(depth))
  assertthat::assert_that(target == 'score' |
                          target == 'pv'    |
                          target == 'bestmove')
  # Get the maximum depth (not always the same for every move)
  get_depths <- function(move_index, gamelog) {
    depths <- stringr::str_match(gamelog[[move_index]], 'info depth ([0-9]+) ')[, 2]
    depths <- unlist(depths)
    max(as.integer(depths), na.rm = TRUE)
  }
  move_index <- 1:length(gamelog)
  depths <- unlist(lapply(move_index, get_depths, gamelog), use.names = FALSE)
  if (!is.null(depth)) {
    temp_depths <- depths[depths != 0]
    if (any(depth > temp_depths)) {
      problem_moves <- which(depth > temp_depths)
      error_message <- paste(
        'Maximum available search depths are ',
        paste0(temp_depths[problem_moves], collapse = ' '),
        '\nfor moves ',
        paste0(problem_moves, collapse = ' '), '.',
        '\n * You requested a depth of ', depth, '.', sep = ''
      )
      stop(error_message)
    }
  }
  # Set the regex
  if (target == 'score') {
    target_regex <- 'score (\\w+\\s-?\\d+)'
  } else if (target == 'pv') {
    target_regex <- ' pv (.*)$'
  } else {
    target_regex <- 'bestmove (\\w*)'
  }
  # Parse the gamelog
  ply <- 1:length(gamelog)
  parse_ply <- function(ply, gamelog, target_regex, depths) {
    depth_lines <- stringr::str_match(gamelog[[ply]], 'info depth ([0-9]+) ')[, 2]
    depth_lines <- as.integer(depth_lines)
    depth <- depths[[ply]]
    result <- stringr::str_match(gamelog[[ply]], target_regex)[, 2]
    if (target == 'bestmove') {
      result <- result[!is.na(result)]
    } else {
      result <- result[which(depth_lines == depth)]
      result <- result[length(result)]
    }
    if (target == 'score') {
      result <- stringr::str_replace(result, 'cp ', '')
    }
    result
  }
  parsed_log <- lapply(ply, parse_ply, gamelog, target_regex, depths)
  lapply(parsed_log, unlist, use.names = FALSE)
}
