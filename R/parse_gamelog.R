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
#' @return A vector of the extracted data.
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
  parse_log <- function(gamelog, depth) {
    # Get the best moves
    bestmoves <- gamelog[grepl('bestmove ', gamelog, fixed = TRUE)]
    bestmoves <- stringr::str_replace(bestmoves, 'bestmove ', '')
    # Remove lines from the log that aren't results
    gamelog <- gamelog[grepl('info depth ', gamelog, fixed = TRUE)]
    # Parse the log
    depths <- stringr::str_match(gamelog, '(?<=depth ).*(?= seldepth)')[, 1]
    if (!is.null(depth)) {
      max_depth <- max(depths)
      if (depth > max_depth) {
        error_message <- paste(
          'Maximum available search depth was ', max_depth, '.',
          '\n * You requested a depth of ', depth, '.', sep = ''
        )
        stop(error_message)
      }
    }
    if (is.null(depth)) {depth <- max(depths)}
    # Parse out the move lines and scores for each PV
    pv_lines <- stringr::str_match(gamelog, '(?<= pv ).*')[, 1]
    pv_lines <- pv_lines[depths == depth]
    pv_scores <- stringr::str_match(gamelog, '(?<=score ).*(?= nodes)')[, 1]
    pv_scores <- stringr::str_replace(pv_scores, 'cp ', '')
    pv_scores <- pv_scores[depths == depth]
    bestmoves <- bestmoves[depths == depth]
    list(score = pv_scores, pv = pv_lines, bestmove = bestmoves)
  }
  processed_gamelog <- lapply(gamelog, parse_log, depth)
  unlist(lapply(processed_gamelog, '[', target), use.names = FALSE)
}
