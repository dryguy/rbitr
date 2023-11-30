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
#' @param append (Default = FALSE) A single-element boolean vector indicating
#'   whether to overwrite or append data to an already existing file.
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
#' autoplay_match('/stockfish.exe', limiter = 'depth', limit = 1, n_cpus = 1,
#' mute = FALSE, ply_limit = 3, n_games = 2)
autoplay_match <- function(engine_path, position = '', limiter, limit, n_cpus,
                           hash_size = NULL, mute = TRUE, ply_limit = NULL,
                           n_games, event = NULL, site = NULL, date = NULL,
                           white = NULL, black = NULL, save_path = NULL,
                           append = FALSE) {
  # Validate the input
  assertthat::assert_that(assertthat::is.count(n_games))

  # Initialize a tibble to store the results of the games
  results <- tibble::tibble(Event    = character(n_games),
                            Site     = character(n_games),
                            Date     = character(n_games),
                            Round    = integer(n_games),
                            White    = character(n_games),
                            Black    = character(n_games),
                            Result   = character(n_games),
                            Movetext = character(n_games))

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

  # Have the chess engine play n_games games against itself
  for (i in seq_len(n_games)) {
    movetext <- autoplay_game(engine_path, position, limiter, limit, n_cpus,
                              hash_size, mute, ply_limit)
    result <- get_result(movetext)
    results[i, ] <- list(event, site, date, i, white, black, result, movetext)
  }

  # Save the games as a PGN file
  if (!is.null(save_path)) {
    save_pgn(results, pgn_path = save_path, append = append)
  }

  # Return the results of the games
  return(results)
}
