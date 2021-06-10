#' Extract data from rbitr's `evaluate_game()` output
#'
#' The `evaluate_game()` function produces a list of engine output for each
#'   position of a chess game. This output tends to be dense, and much of it
#'   will be of no interest to someone analyzing a game. The `parse_gamelog()`
#'   function can extract the parts of interest into a more manageable format.
#'
#' @details Use the `target` parameter to specify what `parse_gamelog()` should return:
#'   positional evaluations in centipawns (score), principal variations (pv), or
#'   best moves (bestmove).
#'
#' @details The data for the deepest available search depth is returned by
#'   default. To extract data for shallower depths, set the `depth`
#'   parameter to the desired depth. Note that if `evaluate_game()` was run
#'   using fixed nodes or fixed time, the deepest search depth will vary across
#'   different positions. If `depth` is set to a value higher than what is
#'   available for some moves, `evaluate_game()` will return an error.
#'
#' @details See
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
#' @seealso
#'   * [rbitr::evaluate_game()] for analyzing individual games.
#'   * [rbitr::evaluate_pgn()] for evaluating all the games in a PGN file.
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
    }
    if (target == 'score') {
      result <- stringr::str_replace(result, 'cp ', '')
    }
    result
  }
  parsed_log <- lapply(ply, parse_ply, gamelog, target_regex, depths)
  lapply(parsed_log, unlist, use.names = FALSE)
}
