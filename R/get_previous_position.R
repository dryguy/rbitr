#' Get the previous position.
#'
#' Trim the last ply from a position string to generate the previous position.
#'
#' @param current_position A single element character vector of moves in
#     long algebraic notation (LAN) representing a chess position.
#'
#' @return A single-element character vector containing a string of moves in LAN
#   notation that is one half-move shorter than the input, representing the
#   preceding chess position.
#' @export
#'
#' @examples
#' movetext <- 'e2e4 g7g5 b1c3 f7f5 d1h5'
#' get_previous_position(movetext)
get_previous_position <- function(current_position) {
  if (identical(current_position, '')) {return(NA)}
  if (length(current_position) > 1 | class(current_position) != 'character') {
    stop(
      paste('current_position must be a character vector of length 1.\n',
            '* It was a', class(current_position), 'of length',
            length(current_position))
    )
  }
  moves <- unlist(
    strsplit(current_position, split = ' ', fixed = TRUE),
    use.names = FALSE
  )
  n_ply <- length(moves)
  # played_move <- moves[n_ply]
  previous_ply <- n_ply - 1
  if (previous_ply <= 0) {
    previous_position <- ''
  } else {
    previous_position <- stringr::str_c(moves[1:previous_ply], collapse = ' ')
  }
  return(previous_position)
}
