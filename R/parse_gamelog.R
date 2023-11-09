#' Extract data from rbitr's `evaluate_game()` output
#'
#' The `evaluate_game()` function produces a list of engine output for each
#'   position of a chess game. This output tends to be dense, and much of it
#'   will be of no interest to someone analyzing a game. The `parse_gamelog()`
#'   function can extract the parts of interest into a more manageable format.
#'
#' @details Use the `target` parameter to specify what `parse_gamelog()` should
#'   return: positional evaluations in centipawns (score), principal variations
#'   (pv), or best moves (bestmove).
#'
#' @details The data for the deepest available search depth is returned by
#'   default. To extract data for shallower depths, set the `depth`
#'   parameter to the desired depth. Note that if `evaluate_game()` was run
#'   using fixed nodes or fixed time, the deepest search depth will vary across
#'   different positions. If `depth` is set to a value higher than what is
#'   available for some moves, `evaluate_game()` will return an error.
#'
#' @details See
#'   [the UCI protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html) for
#'   details of UCI engine output.
#'
#' @param gamelog A list of engine output from rbitr's `evaluate_game()`
#'   function.
#' @param target A single-element character vector of the output to return.
#'   Allowed values are 'score' for the evaluation in centipawns, 'pv' for the
#'   principal variation, or 'bestmove' for the best move.
#' @param depth (Optional, default = NULL) A single-element integer vector
#'   indicating which search depth to return. The value must not be less than 1
#'   or greater than the maximum depth reported by the engine. A value of NULL
#'   returns data for the maximum available depth.
#'
#' @return A list of character vectors of the extracted data, where each list
#'   entry corresponds to a position.
#' @export
#'
#' @inherit tabulate_positionlog seealso
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
  # Return bestmove
  if (target == 'bestmove') {
    return(parse_gamelog_bestmove(gamelog))
  }
  # Return score or pv
  return(parse_gamelog_score(gamelog, target, depth))
}

#' Extract the scores from rbitr's `evaluate_game()` output
#'
#' `parse_gamelog_score()` is an internal function called by
#'   `parse_gamelog()`.
#'
#' @details Returns the engine's calculated evaluation (score) for each
#'   position in the game.  See the documentation for `parse_gamelog()` for
#'   full details.
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
#' @return A list of character vectors of the extracted scores for each
#'   position.
#'
#' @seealso
#'   * [rbitr::parse_gamelog()]
parse_gamelog_score <- function(gamelog, target, depth) {
  # Input validated by parent function `parse_gamelog()`

  # Get the maximum search depth at each position
  # (depth not always the same for every position)
  gamelog_indices <- 1:length(gamelog)
  max_depths <- vapply(gamelog_indices, function(i) {
    depths <- stringr::str_match(gamelog[[i]], 'info depth ([0-9]+) ')[, 2]
    max(as.integer(depths), na.rm = TRUE)
  }, numeric(1))

  # Verify that the requested depth exists at each position
  if (!is.null(depth)) {
    available_depths <- max_depths[max_depths != 0]
    if (any(depth > available_depths)) {
      invalid_depth_requests <- which(depth > available_depths)
      error_message <- paste(
        'Maximum available search depths are ',
        paste0(available_depths[invalid_depth_requests], collapse = ' '),
        '\nfor moves ',
        paste0(invalid_depth_requests, collapse = ' '), '.',
        '\n * You requested a depth of ', depth, '.', sep = ''
      )
      stop(error_message)
    }
  }

  # Use max depth(s) if no depth is specified
  desired_depths <- if (is.null(depth)) {
    max_depths
  } else {
    rep(depth, length(gamelog))
  }

  # Remove lines not containing desired depth(s)
  desired_depth_regexes <- paste0('info depth ', desired_depths)
  delete_unused_lines <- function(gamelog_index, gamelog,
                                  desired_depth_regexes) {
    depth_line_index <-
      stringr::str_detect(gamelog[[gamelog_index]],
                          desired_depth_regexes[[gamelog_index]])
    gamelog[[gamelog_index]] <- gamelog[[gamelog_index]][depth_line_index]
    # Remove lines with no analyses
    multipv_index <- stringr::str_detect(gamelog[[gamelog_index]], 'multipv')
    gamelog[[gamelog_index]] <- gamelog[[gamelog_index]][multipv_index]
  }
  gamelog <- lapply(gamelog_indices, delete_unused_lines, gamelog,
                    desired_depth_regexes)

  # Remove duplicate pvs (leave final pv intact)
  remove_duplicate_pvs <- function(gamelog_indices, gamelog) {
    pv_numbers <- stringr::str_match(gamelog[[gamelog_indices]],
                                     'multipv ([0-9]+)')[, 2]
    rev_pv <- rev(pv_numbers)
    pv_dupe_index <- rev(duplicated(rev_pv))
    gamelog[[gamelog_indices]][!pv_dupe_index]
  }
  gamelog <- lapply(gamelog_indices, remove_duplicate_pvs, gamelog)

  # Parse the gamelog
  parse_gamelog_internal <- function(gamelog_indices, gamelog) {
    if (target == 'score') {
      gamelog[[gamelog_indices]] <-
        stringr::str_match(gamelog[[gamelog_indices]],
                           'score (\\w+\\s-?\\d+)')[, 2]
      gamelog[[gamelog_indices]] <-
        stringr::str_replace(gamelog[[gamelog_indices]], 'cp ', '')
    }
    if (target == 'pv') {
      gamelog[[gamelog_indices]] <-
        stringr::str_match(gamelog[[gamelog_indices]], ' pv (.*)$')[, 2]
    }
    return(gamelog[[gamelog_indices]])
  }
  return(lapply(gamelog_indices, parse_gamelog_internal, gamelog))
}

#' Extract the best moves from rbitr's `evaluate_game()` output
#'
#' `parse_gamelog_bestmove()` is an internal function called by
#'   `parse_gamelog()`.
#'
#' @details Returns the engine's calculated best move (bestmove) for each
#'   position in the game.  See the documentation for `parse_gamelog()` for
#'   full details.
#'
#' @param gamelog A list of engine output from rbitr's `evaluate_game()`
#'   function.
#'
#' @return A list of character vectors of the extracted best moves for each
#'   position.
#'
#' @seealso
#'   * [rbitr::parse_gamelog()]
parse_gamelog_bestmove <- function(gamelog) {
  # Input validated by parent function `parse_gamelog()`
  position_index <- 1:length(gamelog)
  parse_analyses <- function(position_index, gamelog) {
    result <-
      stringr::str_match(gamelog[[position_index]], 'bestmove (\\w*)')[, 2]
    result[!is.na(result)]
  }
  return(vapply(position_index, parse_analyses, character(1), gamelog))
}
