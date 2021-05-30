#' Evaluate each chess game in a pgn file.
#'
#' @details A wrapper for rbitr's `evaluate_game()` function that evaluates each
#'   game in the specified pgn file. Note that this can take a very long time
#'   for pgn files with a lot of games, and even longer when searching high
#'   numbers of principal variations and/or deep evaluations. For long runs, it
#'   is recommended to set save_logs = TRUE. This will save the evaluation of
#'   each game to a folder with the same name as the pgn. If the analysis has to
#'   be stopped and restarted, it will skip games that have already been
#'   analyzed and pick up where it left off.
#'
#' @note The server analysis on lichess.org use a limit of 2250000 nodes. To
#'    mimic this, set limiter = 'nodes', and limit = 2250000.
#'
#' @param pgn_path A single-element character vector of the path to the pgn.
#' @param engine_path A single-element character vector of the path to a UCI
#'    chess engine.
#' @param n_cpus A single-element integer vector of the number of cpus to use.
#' @param n_pv A single-element integer vector of the desired number of
#'   principal variations.
#' @param limiter A single-element character vector indicating the desired
#'   mode of search termination. Allowed values are 'depth' (to search a fixed
#'   number of plies), 'nodes' (to search a fixed number of nodes), and
#'   'movetime' (to search a fixed number of milliseconds).
#' @param limit A single-element integer vector of the desired search depth
#'   (# of plies), search nodes (# of nodes), or search time (# of mseconds).
#' @param mute (Boolean, default = FALSE) Suppress progress report?
#' @param save_logs (Boolean, default = FALSE) Save progress? Recommended for
#'   long analyses in case it has to be stopped before finishing all games.
#'
#' @return A list of gamelogs (see `evaluate_game()` for details).
#' @export
#'
#' @examples
#' library(bigchess)
#' # Modify engine_path as required for your engine location & operating system
#' engine_path <- '//stockfish_13_win_x64_bmi2.exe'
#' pgn_path <- file.path(
#'   system.file(package = 'rbitr'),
#'   'extdata',
#'   'fools_mate.pgn'
#' )
#' evaluate_pgn(pgn_path, engine_path, n_pv = 1, limiter = 'depth', limit = 1)

evaluate_pgn <- function(pgn_path, engine_path, n_cpus = 1L, n_pv, limiter,
                         limit, mute = FALSE, save_logs = FALSE) {
  # Validate input
  assertthat::assert_that(assertthat::is.string(pgn_path))
  assertthat::assert_that(assertthat::is.string(engine_path))
  assertthat::assert_that(assertthat::is.count(n_cpus))
  assertthat::assert_that(assertthat::is.count(n_pv))
  assertthat::assert_that(limiter == 'depth' |
                          limiter == 'nodes' |
                          limiter == 'movetime')
  assertthat::assert_that(assertthat::is.count(limit))
  assertthat::assert_that(assertthat::is.flag(mute))
  assertthat::assert_that(assertthat::is.flag(save_logs))
  # Check for directory of saved progress
  progress_path <- tools::file_path_sans_ext(pgn_path)
  pgn_basename <- basename(pgn_path)
  pgn_basename <- tools::file_path_sans_ext(pgn_basename)
  if (!save_logs & dir.exists(progress_path)) {
    warning(
     paste0('A save directory for ', pgn_basename,' exists.\n',
            'Did you forget to set save_logs = TRUE?')
    )
  }
  if (save_logs & !dir.exists(progress_path)) {
    dir.create(progress_path)
  }
  # Read the pgn
  pgn <- get_pgn(pgn_path)
  # Evaluate the games
  evaluation_loop <- function(row_number, pgn, engine_path, n_pv, limiter,
                              limit, start_time, progress_path, pgn_basename,
                              file_count) {
    # Check for saved progress
    save_path <- file.path(
      progress_path,
      paste0(pgn_basename, '_', limiter, limit, 'pv', n_pv, '_', row_number,
             '.Rdata')
    )
    if (dir.exists(progress_path)) {
      if (file.exists(save_path)) {
        load(save_path)
        return(evaluation)
      }
    }
    # Evaluate the game
    if (!mute) {
      total_rows <- nrow(pgn)
      print(paste0('game ', row_number, ' of ', total_rows))
    }
    movetext <- clean_movetext(pgn$Movetext[row_number])
    evaluation <- evaluate_game(movetext, engine_path, limiter, limit, n_cpus, n_pv)
    # Show progress
    if (!mute) {
      total_time <- difftime(Sys.time(), start_time, units = 'secs')
      time_per_game <- total_time / (row_number - file_count)
      remaining_time <- (total_rows - row_number) * time_per_game
      if (remaining_time < 60) {
        preferred_unit <- 'secs'
      } else if (remaining_time < 3600) {
        preferred_unit <- 'mins'
      } else if (remaining_time < 86400) {
        preferred_unit <- 'hours'
      } else {
        preferred_unit <- 'days'
      }
      remaining_time <- as.numeric(remaining_time, units = preferred_unit)
      print(paste0(
        'estimated time remaining ',
        round(remaining_time, digits = 1),
        ' ',
        preferred_unit
      ))
    }
    # Save results
    if (save_logs) {
      save(evaluation, file = save_path)
    }
    # Return results
    evaluation
  }
  # Run the evaluation loop
  row_numbers <- 1:nrow(pgn)
  start_time <- Sys.time()
  if (dir.exists(progress_path)) {
    existing_filenames <- list.files(progress_path)
    file_regex <- paste0(pgn_basename, '_', limiter, limit, 'pv', n_pv)
    file_count <- sum(stringr::str_detect(existing_filenames, file_regex))
  } else {
    file_count <- 0
  }
  lapply(row_numbers, evaluation_loop, pgn, engine_path , n_pv, limiter,
         limit, start_time, progress_path, pgn_basename, file_count)
}
