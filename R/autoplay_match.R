#' Have a chess engine play a series of games against itself
#'
#' The function `autoplay_match()` is used to make a UCI compatible chess engine
#' play a series of games against itself by repeatedly calling the
#' `autoplay_game()` function. The games are saved in a tibble and optionally as
#' a PGN file.
#'
#' @param engine_path A single-element character vector of the path to a UCI
#'   compatible chess engine.
#' @param position A single-element character vector containing a series of
#'   legal chess moves in long algebraic notation (LAN).
#' @param limiter A single-element character vector indicating the desired mode
#'   of search termination. Allowed values are 'depth' (to search a fixed number
#'   of plies), 'nodes' (to search a fixed number of nodes), and 'movetime' (to
#'   search for a fixed number of milliseconds).
#' @param limit A single-element integer vector of the desired search depth (#
#'   of plies), search nodes (# of nodes), or search time (# of milliseconds).
#' @param n_cpus (Default = 1) A single-element integer vector of the number of
#'   CPUs to use.
#' @param hash_size (Default = NULL) A single-element integer vector of the
#'   desired hash size, in MB.
#' @param mute (Default = TRUE) A boolean indicating if the board should be
#'   printed after each turn.
#' @param ply_limit (Default = NULL) An integer indicating the maximum number of
#'   ply before the game is stopped. Used to limit execution time for unit tests
#'   and examples.
#' @param n_games A single-element integer vector of the number of games to be
#'   played.
#' @param event (Default = NULL) A single-element character vector of the event
#'   name.
#' @param site (Default = NULL) A single-element character vector of the site
#'   name.
#' @param date (Default = NULL) A single-element Date vector of the date of the
#'   game.
#' @param white (Default = NULL) A single-element character vector of the name
#'   of the player playing white.
#' @param black (Default = NULL) A single-element character vector of the name
#'   of the player playing black.
#' @param save_path (Default = NULL) A single-element character vector of the
#'   path where the PGN file should be saved.
#'
#' @return A tibble where each row corresponds to a game played by the chess
#'   engine against itself. The columns of the tibble are `Event`, `Site`,
#'   `Date`, `Round`, `White`, `Black`, `Result`, and `Movetext`.
#'
#' @export
#'
#' @examples
#' # Replace '/stockfish.exe' with a path to your UCI-compatible engine. To play
#' # a full game, delete the ply_limit.
#' autoplay_match('/stockfish.exe', limiter = 'depth', limit = 1, n_cpus = 1L,
#' mute = FALSE, ply_limit = 3, n_games = 2)
autoplay_match <- function(engine_path, position = '', limiter, limit, n_cpus,
                           hash_size = NULL, mute = TRUE, ply_limit = NULL,
                           n_games, event = NULL, site = NULL, date = NULL,
                           white = NULL, black = NULL, save_path = NULL) {
  # Validate the input
  assertthat::assert_that(is.character(engine_path), length(engine_path) == 1)
  assertthat::assert_that(is.character(position))
  assertthat::assert_that(is.character(limiter), length(limiter) == 1)
  assertthat::assert_that(is.numeric(limit), length(limit) == 1)
  assertthat::assert_that(is.numeric(n_cpus), length(n_cpus) == 1)
  assertthat::assert_that(is.null(hash_size) || (is.numeric(hash_size) && length(hash_size) == 1))
  assertthat::assert_that(is.logical(mute), length(mute) == 1)
  assertthat::assert_that(is.null(ply_limit) || (is.numeric(ply_limit) && length(ply_limit) == 1))
  assertthat::assert_that(is.numeric(n_games), length(n_games) == 1)
  assertthat::assert_that(is.null(event) || (is.character(event) && length(event) == 1))
  assertthat::assert_that(is.null(site) || (is.character(site) && length(site) == 1))
  assertthat::assert_that(is.null(date) || (is.character(date) && length(date) == 1))
  assertthat::assert_that(is.null(white) || (is.character(white) && length(white) == 1))
  assertthat::assert_that(is.null(black) || (is.character(black) && length(black) == 1))
  assertthat::assert_that(is.null(save_path) || (is.character(save_path) && length(save_path) == 1))

  # Initialize a tibble to store the results of the games
  results <- tibble::tibble(Event    = character(n_games),
                            Site     = character(n_games),
                            Date     = character(n_games),
                            Round    = integer(n_games),
                            White    = character(n_games),
                            Black    = character(n_games),
                            Result   = character(n_games),
                            Movetext = character(n_games))

  # Check if there is a partially completed PGN file at save_path
  if (!is.null(save_path) && file.exists(save_path)) {
    existing_pgn <- rbitr::get_pgn(save_path)
    existing_pgn$Round <- as.integer(existing_pgn$Round)
    n_existing_games <- nrow(existing_pgn)
    results[seq_len(n_existing_games), ] <- existing_pgn
  } else {
    n_existing_games <- 0
  }

  # Get the engine name
  engine_name <- tools::file_path_sans_ext(basename(engine_path))

  # Set the default values for event, site, date, white, and black
  if (is.null(event)) {
    event <- paste0(engine_name, "_v_", engine_name)
  }
  if (is.null(site)) {
    site <- get_cpu_name()
  }
  if (is.null(date)) {
    date <- format(Sys.Date(), "%Y.%m.%d")
  }
  if (is.null(white)) {
    white <- paste0(engine_name, " White")
  }
  if (is.null(black)) {
    black <- paste0(engine_name, " Black")
  }

  # Have the chess engine play the remaining games against itself
  for (i in (n_existing_games + 1):n_games) {
    movetext <- autoplay_game(engine_path, position = position,
                              limiter = limiter, limit = limit, n_cpus = n_cpus,
                              hash_size = hash_size, mute = mute,
                              ply_limit = ply_limit)
    result <- get_result(movetext)
    results[i, ] <- list(event, site, date, i, white, black, result, movetext)

    # Append the game to the PGN file after each game is completed
    if (!is.null(save_path)) {
      save_pgn(results[i, ], pgn_path = save_path, append = TRUE)
    }
  }

  # Return the results of the games
  return(results)
}
