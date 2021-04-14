#' Extract principal variations (PVs) and scores from UCI chess engine output.
#'
#' Parse the output of a uci-compatible chess engine for the principal
#'   variations and scores.
#'
#' @details The data for the deepest available search depth is returned by
#' default. See
#' [the UCI protocol](http://wbec-ridderkerk.nl/html/UCIProtocol.html) for
#' details of UCI engine output.
#'
#' @param ucilog A character vector of engine output.
#' @param depth (Optional, default = NULL) A single-element integer vector
#'   indicating which search depth to return. The value must not be less than 1
#'   or greater than the maximum depth reported by the engine. A value of NULL
#'   returns data for the maximum depth.
#'
#' @family engine tools
#'
#' @return A recursive list containing the extracted scores and PVs.
#' @export
#'
#' @examples
#' ucilog <- list(c(
#'   "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
#'   "readyok",
#'   "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
#'   "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
#'   "bestmove d2d4"
#' ))
#' parse_ucilog(ucilog)
parse_ucilog <- function(ucilog, depth = NULL) {
  parse_log <- function(ucilog, depth) {
    # Remove lines from ucilog that aren't evaluations.
    ucilog <- ucilog[grepl('info depth ', ucilog, fixed = TRUE)]

    # Parse the log
    depths <- stringr::str_match(ucilog, '(?<=depth ).*(?= seldepth)')[, 1]
    if (!is.null(depth)) {
      max_depth <- max(depths)
      if (depth > max_depth) {
        error_message <- paste(
          'Maximum available search depth was ', max_depth, '.',
          '\n * You requested a depth of ', depth, '.', sep = ''
        )
        stop(error_message)
      }
      if (depth < 1) {
        error_message <- paste(
          'depth must be >= 1.',
          '\n * You requested a depth of ', depth, '.', sep = ''
        )
        stop(error_message)
      }
    }
    if (is.null(depth)) {depth <- max(depths)}

    # Parse out the move lines for each PV
    pv_lines <- stringr::str_match(ucilog, '(?<= pv ).*')[, 1]
    pv_lines <- pv_lines[depths == depth]

    pv_scores <- stringr::str_match(ucilog, '(?<=score ).*(?= nodes)')[, 1]
    pv_scores <- stringr::str_replace(pv_scores, 'cp ', '')
    pv_scores <- pv_scores[depths == depth]
    list(scores = pv_scores, pvs = pv_lines)
  }
  lapply(ucilog, parse_log, depth)
}
