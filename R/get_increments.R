#' Get increments from pgn TimeControl tags
#'
#' Extract the increment from a pgn time control tag. For allowed time control
#'   formats, see the
#'   [pgn specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c9.6)
#'   .
#'
#' @details The `get_increments()` function is intended for use with data read
#'   from a pgn file using the `get_pgn()` function. If a TimeControl tag was
#'   present in the pgn, a tibble obtained using `get_pgn()` will have a
#'   $TimeControl column from which the increment data can be extracted. Note
#'   that there are six different time control formats recognized by the pgn
#'   specification, only one of which uses increments. All other formats will
#'   return an increment of 0 (see example).
#'
#' @param time_control A character vector of pgn TimeControl tags.
#'
#' @return An integer vector of the increments, in seconds. Time controls with
#'   no increment return 0.
#' @export
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
