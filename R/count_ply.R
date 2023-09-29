#' Count the number of ply in a chess game
#'
#' This function takes the movetext of a chess game as input and returns the
#' number of ply (half-moves) in the game.
#'
#' @param movetext A character string containing the movetext of a chess game.
#' @return An integer representing the number of ply in the game.
#' @export
#' @examples
#' count_ply("1. e4 e5 2. Nf3 Nc6")
count_ply <- function(movetext) {
  # Validate input
  assertthat::assert_that(is.character(movetext))

  # Remove annotations, game termination markers, etc.
  movetext <- rbitr::clean_movetext(movetext)

  # Remove move numbers
  movetext <- gsub("\\d+\\.+", "", movetext)

  # Split the movetext into individual moves
  moves <- unlist(strsplit(movetext, " "))

  # Remove empty elements
  moves <- moves[moves != ""]

  # Count the number of moves
  ply <- length(moves)

  return(ply)
}
