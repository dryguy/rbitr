#' Identify chess inaccuracies, mistakes, and blunders
#'
#' The function `get_imb()` identifies inaccuracies, mistakes, and blunders in a
#'    chess game, based on how much the evaluation changes after a move.
#'
#' @details The user must provide a vector of evaluations for the game
#'   via the `scores` parameter. The sign convention is that positive values
#'   indicate white is ahead, while negative values indicate black is ahead.
#'   Evaluation produced using a UCI chess engine use a different convention,
#'   where the score is from the player's point of view. Use the function
#'   `convert_scores` to change the sign convention before passing the
#'   evaluations to `get_imb()`.
#'
#' @details The `scores` parameter should include an evaluation for the initial
#'   position, before white has moved. In addition, a vector of the game's moves
#'   (`moves`), and a vector of the engine's preferred moves (`bestmoves`) must
#'   also be provided. The move data is used to avoid marking a move as an
#'   inaccuracy, mistake, or blunder when it is the best move available. The
#'   `color` parameter is used to indicate which side to evaluate.
#'
#' @details The thresholds for marking moves are based on evaluations after
#'   applying an
#'   [exponential scaling function](https://lichess.org/blog/WFvLpiQAACMA8e9D/learn-from-your-mistakes)
#'   (2 / (1 + exp(-0.004 * scores)) - 1), borrowed from
#'   [lichess.org](lichess.org). This function downplays the impact of
#'   less-than-perfect play when one side is far ahead. The idea being that the
#'   player who is ahead may avoid moves that lead to material gain if those
#'   moves lead to complicated tactics. In such positions players often go for
#'   simple and solid moves that maintain a clear advantage. These types of
#'   moves aren't really blunders. Similarly, a player in a lost position may
#'   make moves designed to complicate the position and create chances for a
#'   comeback, even if the move objectively loses material with perfect play by
#'   the opponent.
#'
#' @details The value of `cap` serves a similar purpose to move scaling. That is
#'   the player with a decisive advantage can have valid reasons not to play the
#'   best move. The cap can be disabled by setting the parameter `cap_action` to
#'   'none'.
#'
#' @details Some engines may give poor evaluations to valid opening moves. To
#'   skip the classification of opening moves, set the `first_ply` parameter to
#'   indicate the desired starting ply. For example, to start from move 8, set
#'   `first_ply` to 15. (The ply count starts with the initial position, so the
#'   beginning of the second move occurs at ply 3, and each subsequent move
#'   begins on an odd ply.) If starting from a position where black is to move,
#'   the parameter `to_move` must be set to 'black'.
#'
#' @note Since the calculations are based on the ones used by
#'   [lichess.org](lichess.org), `get_imb()` should produce numbers very close
#'   to those given by lichess. To replicate the lichess calculation, use the
#'   default settings for `cap` (1000), `cap_action` ('replace'), `first_ply`
#'   (1), `to_move` ('white'), and `mate` (> 1000). In addition, the first
#'   element of `scores` should be an evaluation of the initial position, and be
#'   equal to 15. The results may not be identical to lichess (even if the same
#'   engine is used) since most chess engines are non-deterministic for a
#'   variety of reasons, but they should be fairly close.
#'
#' @param scores A numeric vector of chess engine evaluations. Positions that
#'   are mate in x should be indicated by the same numeric value as the `mate`
#'   parameter (with negative values when black is to give mate).
#' @param moves A character vector of moves in the same format as `bestmoves`.
#' @param bestmoves A character vector of moves in the same format as `moves`.
#' @param color A single-element character vector indicating which side to
#'   analyze. Allowed values are 'black' or 'white'.
#' @param cap (Default = 1000) A single-element numeric vector of the
#'   maximum allowed value for centipawn score. Should always be less than the
#'   setting for mate.
#' @param cap_action (Default = 'replace') A single-element character vector
#'   indicating whether to exclude scores outside the range \[-cap, cap\], to
#'   replace them with the cap, or to not use a cap. Allowed values are
#'   'exclude', 'replace', or 'none'.
#' @param first_ply (Default = 1) A single-element integer vector indicating the
#'   first ply to be included in the calculation. May not be larger than the
#'   number of elements in `score`.
#' @param to_move (Default = 'white') A single-element character vector
#'   indicating which side's turn it is.
#' @param mate (Default = 50000) A single-element numeric vector indicating
#'   what centipawn value was used to represent mate in the `scores` parameter.
#'   Should always be greater than the setting for `cap`. It should also be
#'   greater than the largest non-mating evaluation in the game to avoid
#'   unexpected behavior.
#'
#' @return A named list of move numbers for the $inaccuracies, $mistakes, and
#'   $blunders.
#' @export
#'
#' @seealso
#'   * [rbitr::get_acpl()] for calculating average centipawn loss.
#'   * [rbitr::get_evals()] to load scores from a pgn file.
#'   * [rbitr::get_moves()] to get moves from movetext.
#'   * [rbitr::evaluate_game()] or [rbitr::evaluate_pgn()] to calculate scores.
#'   * [rbitr::convert_scores()] to set values for 'mate x'.
#'   * [rbitr::parse_gamelog()] and [rbitr::parse_pgnlog()] to extract the best
#'     moves from UCI engine output.
#'
#' @examples
#' scores <- c(12, -171, -72, -50000, -50000)
#' moves <- c("g2g4", "e7e6", "f2f4", "d8h4")
#' bestmoves <- c("d2d4", "d7d5", "g1f3", "d8h4")
#' get_imb(scores, moves, bestmoves, color = 'white')

get_imb <- function(scores, moves, bestmoves, color, cap = 1000,
                    cap_action = 'replace', first_ply = 1, to_move = 'white',
                    mate = 50000) {
  # Validate input
  assertthat::assert_that(is.numeric(scores))
  assertthat::assert_that(is.character(moves))
  assertthat::assert_that(is.character(bestmoves))
  assertthat::assert_that(color == 'white' | color == 'black')
  assertthat::assert_that(is.numeric(cap) | is.null(cap))
  assertthat::assert_that(length(cap) == 1 | length(cap) == 0)
  assertthat::assert_that(cap_action == 'exclude' |
                          cap_action == 'replace' |
                          cap_action == 'none')
  assertthat::assert_that(assertthat::is.count(first_ply))
  assertthat::assert_that(max(first_ply, na.rm = TRUE) <= length(scores))
  assertthat::assert_that(assertthat::is.count(min(first_ply, na.rm = TRUE)))
  assertthat::assert_that(to_move == 'white' | to_move == 'black')
  assertthat::assert_that(is.numeric(mate))
  assertthat::assert_that(mate > cap)
  # Whose turn is it?
  if (to_move == 'black') {
    scores <- c(NA, scores)
  }
  # Impose the cap, if any
  if (cap_action == 'replace') {
    scores[scores >  cap & scores <  mate] <-  cap
    scores[scores < -cap & scores > -mate] <- -cap
  } else if (cap_action == 'exclude') {
    scores[scores >  cap] <- NA
    scores[scores < -cap] <- NA
  }
  # Calculate blunders
  ply <- first_ply:length(scores)
  white_ply <- ply[ply %% 2 == 1]
  black_ply <- ply[ply %% 2 == 0]
  wcl <- diff(winning_chances(scores))
  previous_cp <- scores[1:(length(scores) - 1)]
  current_cp <- scores[2:length(scores)]
  if (color == 'white') {
    wcl <- -wcl[white_ply]
    previous_cp <- previous_cp[white_ply]
    current_cp <- current_cp[white_ply]
    moves <- moves[white_ply]
    bestmoves <- bestmoves[white_ply]
    color_index <- 1
  } else if (color == 'black') {
    wcl <- wcl[black_ply]
    previous_cp <- -previous_cp[black_ply]
    current_cp <- -current_cp[black_ply]
    moves <- moves[black_ply]
    bestmoves <- bestmoves[black_ply]
    color_index <- -1
  }
  mid  <-  999
  low  <-  700
  blunders <- unique(c(which(previous_cp ==  mate &
                             current_cp  <=   low &
                             moves != bestmoves),
                       which(current_cp  == -mate &
                             previous_cp >=  -low &
                             moves != bestmoves),
                       which(wcl >= 0.3 &
                             moves != bestmoves)))
  mistakes <- unique(c(which(previous_cp ==  mate &
                             current_cp  <=   mid &
                             current_cp  >    low &
                             moves != bestmoves),
                       which(current_cp  == -mate &
                             previous_cp >=  -mid &
                             previous_cp  <  -low &
                             moves != bestmoves),
                       which(previous_cp !=  mate &
                             current_cp  != -mate &
                             wcl >= 0.2 &
                             wcl  < 0.3 &
                             moves != bestmoves)))
  inaccuracies <- unique(c(which(previous_cp ==  mate &
                                 current_cp  <   mate &
                                 current_cp  >    mid &
                                 moves != bestmoves),
                           which(current_cp  == -mate &
                                 previous_cp  > -mate &
                                 previous_cp  <  -mid &
                                 moves != bestmoves),
                           which(previous_cp !=  mate &
                                 current_cp  != -mate &
                                 wcl >= 0.1 &
                                 wcl  < 0.2 &
                                 moves != bestmoves)))
  blunders <- blunders[order(blunders)]
  mistakes <- mistakes[order(mistakes)]
  inaccuracies <- inaccuracies[order(inaccuracies)]
  return(list(
    inaccuracies = inaccuracies,
    mistakes = mistakes,
    blunders = blunders
  ))
}
