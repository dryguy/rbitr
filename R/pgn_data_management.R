get_clocks <- function (source_movetext) {
  # Extract clock data (if present) from the movetext of a pgn file. For pgns
  # read with bigchess::read.pgn, this data is in the $SourceMovetext column.
  #
  # See http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.2
  # for the specification of pgn movetext.
  #
  # Both lichess.org and chess.com store clock data in the format [%clk h:mm:ss]
  #
  # Arguments:
  #   source_movetext: A single-element character vector of pgn movetext
  #
  # Returns:
  #   A numeric vector of the time remaining after each move, in seconds

  clock <- lapply(
    stringr::str_match_all(source_movetext, '\\[%clk\\s*([^]]*)'), '[', , 2
  )
  clock <- lapply(clock, as.difftime, units='secs')
  unlist(lapply(clock, as.numeric), use.names = FALSE)
}

get_increments <- function (time_control) {
  # Extract the increment from a pgn time control tag. For allowed time control
  # formats, see:
  # http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c9.6
  #
  # Arguments:
  #   time_control:
  #     A single-element character vector of a valid pgn time control tag
  #
  # Returns:
  #   A single-element integer vector of the increment, in seconds. If there is
  #   no increment, return 0.

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
