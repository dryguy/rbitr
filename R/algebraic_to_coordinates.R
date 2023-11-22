#' Convert algebraic notation to coordinates
#'
#' This function converts a square's algebraic notation (e.g., "e5") to a pair
#' of coordinates (e.g., c(5, 5)).
#'
#' @param square A character string giving the algebraic notation of the square.
#'
#' @return A numeric vector of length 2 giving the coordinates of the square.
#'   The first element is the row number and the second element is the column number.
#' @examples
#' algebraic_to_coordinates("e5")  # Returns c(5, 5)
#' @export
algebraic_to_coordinates <- function(square) {
  # Validate input
  assertthat::assert_that(is.character(square))
  assertthat::assert_that(nchar(square) == 2)

  # Define the mapping from letters to numbers
  letters_to_numbers <- c(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8)

  # Extract the letter and number from the square
  letter <- substr(square, 1, 1)
  number <- as.numeric(substr(square, 2, 2))

  # Convert the letter to a number
  column <- letters_to_numbers[[letter]]

  # Return the coordinates
  return(c(number, column))
}
