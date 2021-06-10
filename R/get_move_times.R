#' Convert clock data to move times
#'
#' Clock data (in seconds) is converted into move times (in seconds).
#'
#' @details Clock times and increments are typically obtained using rbitr's
#'   `get_clocks()` and `get_increments()` functions. The move times start from
#'   the second move (the first move is taken to be zero seconds).
#'
#' @note The author has encountered PGN files where a move resulted in the
#'   remaining time increasing by more than the increment value, giving a
#'   negative move times. This might occur due to a software bug causing the
#'   time to be incorrectly recorded, or perhaps by the action of an arbiter
#'   (for example, when adding time to an opponents clock as a penalty). If
#'   `get_move_times()` finds a negative move time, it will take action as
#'   directed by the `negative_time` parameter. A value of 'NA' will treat
#'   negative move times as missing data, replacing them with NAs. A value of
#'   'no_action' will leave negative move times in place, treating them as
#'   unrecorded additions of time to the clock.
#'
#' @param clock A numeric vector of chess clock times at the end of each half
#'   move (in seconds).
#' @param increment A single-element numeric vector of the increment time (in
#'   seconds).
#' @param color A character vector indicating which side to calculate (either
#'   black or white).
#' @param negative_time (Default = 'NA') A character vector indicating how to
#'   treat negative move times. If negative_time = 'NA', negative times will
#'   be replaced with NAs. A value of 'no action' will leave negative move times
#'   in place.
#'
#' @return A numeric vector of the move times in seconds. The vector starts at
#'   the second move (the first move would count as zero seconds).
#' @export
#'
#' @seealso
#'   * [rbitr::get_pgn()] to load the time control and other data from a PGN
#'     file.
#'   * [rbitr::get_clocks()] to extract the clock data from a PGN file.
#'   * [rbitr::get_increments()] to extract increment data from a PGN file.
#'   * [rbitr::scaled_time_plot()] and [rbitr::game_summary_plot()] to plot move
#'     times.
#'
#' @examples
#' clock <- c(900, 900, 888, 878, 878, 858)
#' get_move_times(clock, 8, 'white')
#' get_move_times(clock, 8, 'black')

get_move_times <- function(clock, increment, color, negative_time = 'NA') {
  assertthat::assert_that(is.numeric(clock))
  assertthat::assert_that(is.numeric(increment))
  assertthat::assert_that(increment >= 0)
  assertthat::assert_that(length(increment) == 1)
  assertthat::assert_that(color == 'white' | color == 'black')
  assertthat::assert_that(negative_time == 'NA' | negative_time == 'no action')
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
  if (any(move_times < 0, na.rm = TRUE) & negative_time == 'NA') {
    move_times[move_times < 0] <- NA_real_
  }
  c(0, as.numeric(move_times))
}
