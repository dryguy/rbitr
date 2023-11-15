#' Convert a chess move in long algebraic notation to coordinates
#'
#' @param lan A string representing a chess move in
#'   [UCI](http://wbec-ridderkerk.nl/html/UCIProtocol.html) long algebraic
#'   notation (e.g., "e2e4", "e7e8q").
#' @return A list containing the original coordinates, destination coordinates,
#'   and the promoted piece (if any).
#' @export
#' @examples
#' lan_to_coordinates("e2e4")
#' lan_to_coordinates("e7e8q")
lan_to_coordinates <- function(lan) {
  # Validate input
  if (!grepl("^[a-h][1-8][a-h][1-8][qrbnQRBN]?$", lan)) {
    rlang::abort(glue::glue("`lan` must be a valid chess move in UCI long algebraic notation. Invalid input was: {lan}"),
          .subclass = "lan_to_coordinates_error")
  }

  # Extract original and destination coordinates
  original_coordinates    <- strsplit(substr(lan, 1, 2), "")[[1]]
  destination_coordinates <- strsplit(substr(lan, 3, 4), "")[[1]]

  # Convert letters to numbers
  letters_to_numbers <- function(x) {
    return(match(x, letters))
  }
  original_coordinates[1]    <- letters_to_numbers(original_coordinates[1])
  destination_coordinates[1] <- letters_to_numbers(destination_coordinates[1])

  # Convert character to integer
  original_coordinates    <- as.integer(original_coordinates)
  destination_coordinates <- as.integer(destination_coordinates)

  # Check for promotion
  if (nchar(lan) == 5) {
    promoted_piece <- substr(lan, 5, 5)
  } else {
    promoted_piece <- NULL
  }

  # Return list
  return(list(origin    = original_coordinates,
              target    = destination_coordinates,
              promotion = promoted_piece))
}
