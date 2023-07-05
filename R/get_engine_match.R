#' Determine the ratio of a player's moves that match an engine's moves
#'
#' The `get_engine_match()` function calculates the fraction of moves played
#'   that correspond to the 'best' move as determined by a chess engine.
#'
#' @details The `get_engine_match()` function requires a gamelog in the same
#'   format as the one returned by `evaluate_game()`. It also requires the
#'   moves of the game to be supplied in the same format provided by `get_pgn()`.
#'
#' @details If desired, the analysis depth used to determine the
#'   best move may be specified using the `depth` parameter. If `depth` is set
#'   to a value higher than what is available for some moves, `evaluate_game()`
#'   will return an error. If no value for `depth` is provided, the deepest
#'   analysis at each ply will be used.
#'
#' @details The ratio is determined by counting the number of moves that are the
#'   same as the engine's preferred moves, and then dividing by the total number
#'   of moves in the game:
#'     bestmove_ratio = # of moves matching the engine / total # of moves
#'
#' @note By itself, observing a player to have a high percentage of moves that
#'   are the same as engine moves is *not* necessarily evidence of illicit
#'   computer assistance. Having a high percentage can be a consequence of a
#'   game having many *forcing* lines. It is also the case that if many games
#'   are analyzed, it becomes likely that some of them will have high matching
#'   percentages by chance alone. Prof. Ken Regan calls this
#'   [the parable of the golfers](https://cse.buffalo.edu/~regan/chess/fidelity/Golfers.html).
#'
#' @param gamelog A list of engine output from rbitr's `evaluate_game()`
#'   function.
#' @param movetext A single-element character vector containing a sequence of
#'   moves in standard algebraic notation (SAN).
#' @param depth (Optional, default = NULL) A single-element integer vector
#'   indicating which search depth to return. The value must not be less than 1
#'   or greater than the maximum depth reported by the engine. A value of NULL
#'   returns data for the maximum available depth.
#'
#' @return A list containing the ratio of engine-matching moves to the total
#'   number of moves for both white ($white_ratio) and black ($black_ratio).
#' @export
#'
#' @seealso
#'   * [rbitr::get_pgn()] for loading the movetext from a pgn file.
#'   * [rbitr::evaluate_game()] for creating a gamelog.
#'
#' @examples
#' movetext <- '1. e4 g5 2. Nc3 f5 3. Qh5# 1-0'
#' # Modify engine_path as required for your engine location & operating system
#' engine_path <- '//stockfish.exe'
#' gamelog <- evaluate_game(movetext, engine_path, n_pv = 1, limiter = 'depth',
#'                          limit = 1)
#' get_engine_match(gamelog, movetext)
get_engine_match <- function(gamelog, movetext, depth = NULL) {
  # Validate input
  assertthat::assert_that(is.list(gamelog))
  assertthat::assert_that(assertthat::is.count(depth) | is.null(depth))
  # Count the matches
  bestmoves <- unlist(parse_gamelog(gamelog, 'bestmove'))
  moves <- get_moves(movetext)[[1]]
  n_ply <- length(moves)
  bestmoves <- bestmoves[1:n_ply]
  matches <- moves == bestmoves
  white_ply <- seq.int(from = 1L, to = n_ply, by = 2L)
  if (n_ply < 2) {
    black_ply <- NA
  } else {
   black_ply <- seq.int(from = 2L, to = n_ply, by = 2L)
  }
  white_bestmoves <- bestmoves[white_ply]
  black_bestmoves <- bestmoves[black_ply]
  white_moves <- moves[white_ply]
  black_moves <- moves[black_ply]
  white_matches <- white_moves == white_bestmoves
  black_matches <- black_moves == black_bestmoves
  n_white_match <- sum(white_matches)
  n_black_match <- sum(black_matches)
  n_white_ply <- length(white_ply)
  n_black_ply <- length(black_ply)
  # Return the ratios.
  white_ratio <- n_white_match / n_white_ply
  black_ratio <- n_black_match / n_black_ply
  return(list(white_ratio = white_ratio, black_ratio = black_ratio,
              white_matches = white_matches, black_matches = black_matches,
              matches = matches))
}
