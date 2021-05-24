#' Identify inaccuracies, mistakes, and blunders.
#'
#' The function `get_imb()` is meant to mimic the inaccuracy, mistake, and
#'   blunder stats provided by the analysis tool on
#'   [lichess.org](http://lichess.org). When analyzing using the same engine and
#'   the same engine settings that lichess uses, `get_imb()` should produce
#'   numbers very close to those given by lichess. Some differences are expected
#'   however, since most chess engines are non-deterministic for a variety of
#'   reasons.
#'
#' @note The engine used by lichess.org changes from time to time as engine
#'   technology improves. Refer to their website for information on the engine
#'   currently in use.
#'
#' @details The user must provide a vector of evaluations for the game
#'   (`scores`). These should include an evaluation for the initial position,
#'   before white has moved. In addition, a vector of the game's moves
#'   (`moves`), and a vector of the engine's preferred moves (`bestmoves`) must
#'   also be provided. The move data is used to avoid marking the best available
#'   move as an inaccuracy, mistake, or blunder. The `color` parameter is used
#'   to indicate which side to evaluate.
#'
#' @param scores A numeric or integer vector of chess engine evaluations.
#' @param moves A character vector of moves in the same format as `bestmoves`.
#' @param bestmoves A character vector of moves in the same format as `moves`.
#' @param color A single-element character vector indicating which side to
#'   analyze. Allowed values are 'black' or 'white'.
#' @param cap (Default = 1000) A single-element integer or numeric vector of the
#'   maximum allowed value for centipawn score. The default of 1000 centipawns
#'   is the value used by lichess.
#' @param cap_action (Default = 'replace') A single-element character vector
#'   indicating whether to exclude scores outside the range \[-cap, cap\], or to
#'   replace them with the cap. Allowed values are 'exclude', 'replace', or
#'   'none'. The default of 'replace' is required to mimic lichess.
#' @param first_ply (Default = 1) A single-element integer vector indicating the
#'   first ply to be included in the calculation. May not be larger than the
#'   number of elements in `score`. A value of 1 is required to mimic lichess.
#' @param to_move (Default = 'white') A single-element character vector
#'   indicating which side's turn it is. The default value of 'white' is
#'   required to mimic lichess.
#' @param mate (Default = 50000) A single-element integer or numeric vector of
#'   the centipawn value to assign for mate. Should always be set higher than
#'   the setting for cap. It should also be higher than the highest numerical
#'   evaluation in the game to avoid unexpected behaviour.
#'
#' @return A named list of $inaccuracies, $mistakes, and $blunders.
#' @export
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
  #mate <- 5000
  mid  <-  999
  low  <-  700
  if (length(current_cp) != length(moves)) {
    print('moves != current_cp')
    print(moves)
    print(current_cp)
  }
  if (length(bestmoves) > length(moves)) {
    bestmoves <- bestmoves[1:length(moves)]
  }
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
  return(list(
    inaccuracies = inaccuracies,
    mistakes = mistakes,
    blunders = blunders
  ))
}
