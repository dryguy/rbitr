convert_scores <- function(scores) {
  # Validate input
  assertthat::assert_that(is.character(scores))
  # Convert 'mate x' to numeric value
  ply <- 1:length(scores)
  mate0 <- which(stringr::str_detect(scores, 'mate 0'))
  if (ply[mate0] %% 2 == 1) {
    scores[mate0] <- 5000
  }
  if (ply[mate0] %% 2 == 0) {
    scores[mate0] <- -5000
  }
  scores[stringr::str_detect(scores, 'mate [0-9]+')] <- 5000
  scores[stringr::str_detect(scores, 'mate -[0-9]+')] <- -5000
  # Convert upperbound & lowerbound to numeric
  bounds <- stringr::str_detect(scores, 'bound')
  if (any(bounds, na.rm = TRUE)) {
    scores[which(bounds)] <- stringr::str_replace(scores[which(bounds)],
                                                  ' [a-z]*bound', '')
  }
  # Convert characters to integer
  scores <- as.integer(scores)
  # Convert signs so that if score > 0 white is winning.
  index <- 1:length(scores)
  scores[index %% 2 == 0] <- -scores[index %% 2 == 0]
  scores
}
