#' Get increments from a PGN file's TimeControl tags
#'
#' Extract the increments from a PGN file's time control tags. For allowed time
#'   control formats, see the
#'   [PGN specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c9.6)
#'   .
#'
#' @details The `get_increments()` function is intended for use with data that
#'   was read from a PGN file with the `get_pgn()` function. If a TimeControl
#'   tag was present in the PGN file, a tibble obtained using `get_pgn()` will
#'   have a $TimeControl column from which the increment data can be extracted.
#'   Note that there are six different time control formats recognized by the
#'   PGN specification, only one of which uses increments. All other formats
#'   will return an increment of 0 (see example).
#'
#' @param time_control A character vector of TimeControl tags.
#'
#' @return An integer vector of the increments, in seconds. Time controls with
#'   no increment return 0.
#' @export
#'
#' @seealso
#'   * [rbitr::get_pgn()] to load the time control and other data from a PGN
#'     file.
#'   * [rbitr::get_clocks()] to extract the clock data from a PGN file.
#'   * [rbitr::get_move_times()] to calculate move times from clock and
#'     increment data.
#'   * [rbitr::time_plot()] and [rbitr::game_summary_plot()] to plot move
#'     times.
#'
#' @examples
#' time_control <- c('?', '-', '40/9000', '300', '4500+60', '*180')
#' get_increments(time_control)
get_increments <- function(time_control) {
  # Validate input
  assertthat::assert_that(is.character(time_control))
  # For formats besides 'base+increment', return 0
  plus_count <- stringr::str_count(time_control, stringr::fixed('+'))
  increment <- rep(0, length(time_control))
  # Return the increments
  increment[plus_count == 1] <- unlist(
    lapply(
     strsplit(time_control[plus_count == 1], split = '+', fixed = TRUE), '[[', 2
    ),
    use.names = FALSE
  )
  as.integer(increment)
}
