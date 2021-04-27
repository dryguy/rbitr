#' Extract data from the output of rbitr's `evaluate_game()` function.
#'
#' The output of rbitr's `evaluate_game()` function consists of a list of UCI
#'   engine output, where each entry in the list is for a single position. The
#'   evaluations and variations are embedded within a lot of other data. The
#'   `parse_gamelog()` function can extract positional evaluations in centipawns
#'   (score), principal variations (pv), or best moves (bestmove).
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
#' @return A list of vectors of the extracted data, where each list entry
#'   corresponds to a position.
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
  # Get the maximum depth
  depths <- stringr::str_match(gamelog[[1]], 'depth (.*) seldepth')[, 2]
  depths <- as.integer(depths)
  if (!is.null(depth)) {
    max_depth <- max(depths, na.rm = TRUE)
    if (depth > max_depth) {
      error_message <- paste(
        'Maximum available search depth was ', max_depth, '.',
        '\n * You requested a depth of ', depth, '.', sep = ''
      )
      stop(error_message)
    }
  } else {
    depth <- max(depths, na.rm = TRUE)
  }
  # Set the regex
  if (target == 'score') {
    target_regex <- ' score (.*) nodes'
  } else if (target == 'pv') {
    target_regex <- ' pv (.*)$'
  } else {
    target_regex <- 'bestmove (\\w*)'
  }
  # Parse the gamelog
  ply <- 1:length(gamelog)
  parse_ply <- function(ply, gamelog, target_regex, depth) {
    depths <- stringr::str_match(gamelog[[ply]], 'depth (.*) seldepth')[, 2]
    depths <- as.integer(depths)
    result <- stringr::str_match(gamelog[[ply]], target_regex)[, 2]
    if (target_regex == 'bestmove (\\w*)') {
      result <- result[!is.na(result)]
    } else {
      result <- result[which(depths == depth)]
    }
    if (target_regex == ' score (.*) nodes') {
      result <- stringr::str_replace(result, 'cp ', '')
    }
    result
  }
  parsed_log <- lapply(ply, parse_ply, gamelog, target_regex, depth)
  lapply(parsed_log, unlist, use.names = FALSE)
}
