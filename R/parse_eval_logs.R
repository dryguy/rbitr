#' Parse Evaluation Logs
#'
#' @param eval_logs A list of ucilogs.
#' @param depth (Optional, default = NULL) A single-element integer vector
#'   indicating which search depth to return. The value must not be less than 1
#'   or greater than the maximum depth reported by the engine. A value of NULL
#'   returns data for the maximum depth.
#'
#' @return A recursive list of ucilogs.
#' @export
#'
#' @examples
#' eval_logs <- list(list(c(
#'   "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
#'   "readyok",
#'   "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
#'   "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
#'   "bestmove d2d4"
#' )))
#' parse_eval_logs(eval_logs)
parse_eval_logs <- function(eval_logs, depth = NULL) {
  lapply(eval_logs, parse_ucilog, depth)
}
