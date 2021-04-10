#' Get clock data from pgn movetext
#'
#' Extract clock data (if present) from the movetext of a pgn file.
#'
#' @details See http://www.enpassant.dk/chess/palview/enhancedpgn.htm for the
#'   specification of the clk command in pgn files.
#'
#' @details For pgns read with `bigchess::read.pgn` or `rbitr::read_pgn`, the
#'   Movetext column is stripped of embedded commands and will not contain clock
#'   data. The original, unmodified movetext is found in the SourceMovetext
#'   column.
#'
#' @param source_movetext A character vector of pgn movetext.
#'
#' @return A list containing numeric vectors of time remaining after each move,
#' in seconds.
#' @export
#'
#' @family pgn data manipulators
#'
#' @examples
#' source_movetext <- c(
#'   '1. d4 {[%clk 0:15:00]} d5 {[%clk 0:15:00]} 2. Nc3 {[%clk 0:14:48]}',
#'   '1. e4 {[%clk 0:15:00]} e5 {[%clk 0:15:00]} 2. Nf3 {[%clk 0:14:38]}'
#' )
#' get_clocks(source_movetext)
get_clocks <- function(source_movetext) {
  clock <- lapply(
    stringr::str_match_all(source_movetext, '\\[%clk\\s*([^]]*)'), '[', , 2
  )
  clock <- lapply(clock, as.difftime, units = 'secs')
  lapply(clock, as.numeric)
}

#' Get increments from pgn TimeControl
#'
#' Extract the increment from a pgn time control tag. For allowed time control
#'   formats, see:
#'   http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c9.6
#'
#' @param time_control A character vector of pgn TimeControl tags.
#'
#' @return An integer vector of the increments, in seconds. Time controls with
#' no increment return 0.
#' @export
#'
#' @family pgn data manipulators
#'
#' @examples
#' time_control <- c('?', '-', '40/9000', '300', '4500+60', '*180')
#' get_increments(time_control)
get_increments <- function(time_control) {
  # For formats besides 'base+increment', return 0
  plus_count <- stringr::str_count(time_control, stringr::fixed('+'))
  increment <- rep(0, length(time_control))

  # Return the increments
  increment[plus_count == 1] <- unlist(
    lapply(
      strsplit(time_control[plus_count == 1], split='+', fixed = TRUE), '[[', 2
    ),
    use.names = FALSE
  )
  as.integer(increment)
}

#' Convert clock data to move times
#'
#' Clock data (in seconds) is converted into move times (in seconds).
#'
#' @details Clock times are typically obtained from `rbitr::get_clocks`, and
#' increment times from `rbitr::get_increments`. The move times start from the
#' second move (the first move is taken to be zero seconds).
#'
#' @note Data from lichess.org may be subject to errors due to unrecorded
#' additions of time from take backs, or from the opponent granting more time.
#' If either of these events occur in a given move, then the calculated move
#' time will be shorter than the actual move time. This error can only be
#' detected when it results in a negative move time, in which case the value is
#' replaced with NA.
#'
#' @param clock An integer vector of chess clock times at the end of each half
#'   move (in seconds).
#' @param increment A single-element integer vector of the increment time (in
#'   seconds).
#' @param color A character vector indicating which side to calculate (either
#'   black or white).
#'
#' @return A numeric vector of the move times (if present) in seconds. The
#'   vector starts at the second move (the first move is zero seconds).
#' @export
#'
#' @family pgn data manipulators
#'
#' @examples
#' clock <- c(900, 900, 888, 878, 878, 858)
#' calculate_move_times(clock, 8, 'white')
#' calculate_move_times(clock, 8, 'black')
calculate_move_times <- function (clock, increment, color) {
  # Extract clocks from the odd ply for white or the even ply for black
  n_ply <- length(clock)
  if (n_ply == 0) {
    return(numeric(0))
  } else if (color == 'white') {
    ply <- seq.int(from = 1L, to = n_ply, by = 2L)
  } else if (color == 'black') {
    ply <- seq.int(from = 2L, to = n_ply, by = 2L)
  } else {
    stop('Allowed colors are "white" or "black".')
  }
  move_times <- increment - diff(clock[ply])
  # Change negative move times to NA.
  move_times[move_times < 0] <- NA
  move_times
}
