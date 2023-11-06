#' Generate positions from movetext
#'
#' This function takes a string containing chess moves in standard algebraic
#' notation (SAN) and returns a character vector of positions, also in SAN. The
#' initial position is represented by an empty string "". Subsequent positions
#' are represented by the series of chess moves that generated the position.
#' This is mainly useful for generating positions that can be fed to a chess
#' engine for analysis.
#'
#' The function `get_positions` expects the input to be valid SAN that conforms
#' to the
#' [PGN specification](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm).
#' It does not perform any checks to ensure that the input is valid and consists
#' of legal chess moves. Annotations, game termination markers, comments, etc.,
#' will be removed from the output.
#'
#' @param movetext A string containing chess moves in SAN.
#'
#' @return A character vector of chess positions.
#'
#' @examples
#' movetext <- "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6"
#' positions <- get_positions(movetext)
#' print(positions)
#' @export
get_positions <- function(movetext) {
  # Validate input
  assertthat::assert_that(assertthat::is.string(movetext))

  # Handle empty string
  if (movetext == '') {
    return('')
  }

  # Remove annotations, game termination markers, etc.
  movetext <- rbitr::clean_movetext(movetext)

  # Remove move numbers
  movetext <- gsub("\\d+\\.+", "", movetext)
  movetext <- gsub(" +", " ", movetext)
  movetext <- trimws(movetext)

  # Split the movetext into individual moves
  moves <- unlist(strsplit(movetext, " "))

  # Initialize an empty vector to store the positions
  positions <- character(length(moves) + 1)

  # Construct the positions by cumulatively concatenating the moves
  for (i in 1:length(moves)) {
    if (i %% 2 == 1) {
      move_number <- paste0(' ', floor(i/2) + 1, '.')
    } else {
      move_number <- ''
    }
    positions[i + 1] <- paste0(positions[i], move_number, ' ', moves[i])
  }

  positions <- trimws(positions)
  return(positions)
}
