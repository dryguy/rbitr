#' Convert clock data to move times
#'
#' Clock data (in seconds) is converted into move times (in seconds).
#'
#' @details Clock times and increments are typically obtained using rbitr's
#' `get_clocks()`, and `get_increments()` functions. The move times start from
#' the second move (the first move is taken to be zero seconds).
#'
#' @note The author has noted that clock data in pgns from lichess.org may
#' sometimes give negative move times. The author speculates that these errors
#' may be due to unrecorded additions of time during the game. Since lichess has
#' options to allow moves to be taken back, or for the opponent to grant more
#' time during the game, it may be possible that these actions are not being
#' accurately reflected in the clock times recorded in the pgn. If unrecorded
#' additions of time were to occur in a given move, then the calculated move
#' time will be shorter than the actual move time. Such an error would only be
#' detected if it resulted in a negative move time. Whatever the cause, in the
#' rare cases where negative move times are encountered, the negative value is
#' replaced with NA.
#'
#' @param clock An integer or numeric vector of chess clock times at the end of
#'   each half move (in seconds).
#' @param increment A single-element integer or numeric vector of the increment
#'   time (in seconds).
#' @param color A character vector indicating which side to calculate (either
#'   black or white).
#'
#' @return A numeric vector of the move times (if present) in seconds. The
#'   vector starts at the second move (the first move is zero seconds).
#' @export
#'
#' @examples
#' clock <- c(900, 900, 888, 878, 878, 858)
#' get_move_times(clock, 8, 'white')
#' get_move_times(clock, 8, 'black')
get_move_times <- function(clock, increment, color) {
  assertthat::assert_that(is.numeric(clock))
  assertthat::assert_that(is.numeric(increment))
  assertthat::assert_that(increment >= 0)
  assertthat::assert_that(length(increment) == 1)
  assertthat::assert_that(color == 'white' | color == 'black')
  # Extract clocks from the odd ply for white or the even ply for black
  n_ply <- length(clock)
  if (n_ply == 0) {
    return(numeric(0))
  } else if (color == 'white') {
    ply <- seq.int(from = 1L, to = n_ply, by = 2L)
  } else if (color == 'black') {
    ply <- seq.int(from = 2L, to = n_ply, by = 2L)
  }
  move_times <- increment - diff(clock[ply])
  # Change negative move times to NA.
  move_times[move_times < 0] <- NA_real_
  as.numeric(move_times)
}
