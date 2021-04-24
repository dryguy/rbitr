convert_scores <- function(scores) {
  # Validate input
  assertthat::assert_that(is.character(scores))
  # Convert 'mate x' to numeric value
  scores[stringr::str_detect(scores, 'mate [0-9]+')] <- 5000
  scores[stringr::str_detect(scores, 'mate -[0-9]+')] <- -5000
  # Convert characters to integer
  scores <- as.integer(scores)
  # Convert signs so that if score > 0 white is winning.
  index <- 1:length(scores)
  scores[index %% 2 == 0] <- -scores[index %% 2 == 0]
  scores
}
