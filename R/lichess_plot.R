#' Lichess plot
#'
#' Generate a plot of game statistics in the style of
#'   [lichess.org](http://lichess.org).
#'
#' The function`lichess_plot()` will first look for existing analysis in the pgn
#'   file, and if not found, it will also look for saved analysis. If no
#'   analysis data is found, it will analyze the game to a depth of 2250000
#'   nodes.
#'
#' @param pgn_path A single-element character vector of the path to a pgn file.
#' @param game_number A single-element integer vector indicating which game in
#'   the pgn to plot.
#' @param engine_path A single-element character vector of the path to a UCI
#'   compatible chess engine.
#' @param n_cpus (Default = 1) A single-element integer vector indicating how
#'   many cpus to use for analysis.
#' @param use_pgn_evals (Default = TRUE) A single-element boolean indicating
#'   whether to use evals from the pgn, if present.
#'
#' @return A ggplot object of the plotted data.
#' @export
#'
#' @examples
#' pgn_path <- file.path(
#'   system.file(package = 'rbitr'),
#'   'extdata',
#'   'short_game.pgn'
#' )
#' # Modify engine_path as required for your engine location & operating system
#' engine_path <- '//stockfish_13_win_x64_bmi2.exe'
#' lichess_plot(pgn_path, game_number = 1, engine_path)
lichess_plot <- function(pgn_path, game_number, engine_path, n_cpus = 1,
                         use_pgn_evals = TRUE) {
  # Validate input
  assertthat::assert_that(assertthat::is.string(pgn_path))
  assertthat::assert_that(assertthat::is.count(game_number))
  assertthat::assert_that(assertthat::is.string(engine_path))
  assertthat::assert_that(assertthat::is.count(n_cpus))
  assertthat::assert_that(assertthat::is.flag(use_pgn_evals))
  # Load the game
  pgn <- get_pgn(pgn_path)
  assertthat::assert_that(game_number <= nrow(pgn))
  # Get the moves
  moves <- get_moves(pgn$Movetext[[game_number]])
  n_ply <- 1:length(moves)
  # Load the clocks
  clocks <- get_clocks(pgn$Movetext[[game_number]])[[1]]
  increment <- get_increments(pgn$TimeControl[[game_number]])
  white_move_times <- get_move_times(clocks, increment, 'white')
  black_move_times <- get_move_times(clocks, increment, 'black')
  # Get evals
    # Look in pgn first
  evals <- get_evals(pgn$Movetext[[game_number]])[[1]]
  evals <- c(15, evals)
    # Then look for analysis log; analyze game if log is missing
  progress_path <- tools::file_path_sans_ext(pgn_path)
  pgn_basename <- basename(pgn_path)
  pgn_basename <- tools::file_path_sans_ext(pgn_basename)
  save_path <- file.path(
    progress_path,
    paste0(pgn_basename, '_', 'nodes', '2250000', 'pv', '1', '_', game_number,
           '.Rdata')
  )
  if (file.exists(save_path)) {
    load(save_path)
  } else {
    evaluation <- evaluate_game(pgn$Movetext[[game_number]], engine_path, n_pv = 1,
                                go_mode = 'nodes', go_value = 2250000)
  }
  if (!use_pgn_evals & !identical(evals, list(numeric(0)))) {
    evals <- parse_gamelog(evaluation, target = 'score')
    evals <- unlist(evals)
    evals <- convert_scores(evals)
  }
  bestmoves <- parse_gamelog(evaluation, target = 'bestmove')
  bestmoves <- unlist(bestmoves)
  # Make the plots
  p1 <- lichess_time_plot(white_move_times, black_move_times)
  p2 <- lichess_advantage_plot(evals)
  # Calculate stats
  moves <- get_moves(pgn$Movetext[[game_number]])[[1]]
  white_imb <- get_imb(evals, moves, bestmoves, 'white')
  black_imb <- get_imb(evals, moves, bestmoves, 'black')
  white_acpl <- get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')
  black_acpl <- get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')
  white_table <- data.frame(
    c('\u25CB', length(white_imb$inaccuracies), length(white_imb$mistakes),
      length(white_imb$blunders), get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')),
    c(pgn$White[[game_number]], 'inaccuracies', 'mistakes', 'blunders', 'Average cewntipawn loss')
  )
  black_table <- data.frame(
    c('\u25CF', length(black_imb$inaccuracies), length(black_imb$mistakes),
      length(black_imb$blunders), get_acpl(evals, 'black', cap = 1000, cap_action = 'replace')),
    c(pgn$Black[[game_number]], 'inaccuracies', 'mistakes', 'blunders', 'Average centipawn loss')
  )
  white_col_a <- c('\u25CB',
                   length(white_imb$inaccuracies),
                   length(white_imb$mistakes),
                   length(white_imb$blunders),
                   get_acpl(evals, 'white', cap = 1000, cap_action = 'replace'))
  white_col_b <- c(pgn$White[[game_number]],
                   'inaccuracies', 'mistakes', 'blunders',
                   'Average cewntipawn loss')
  white_caption <- make_table(white_col_a, white_col_b)
  black_col_a <- c('\u25CF',
                   length(black_imb$inaccuracies),
                   length(black_imb$mistakes),
                   length(black_imb$blunders),
                   get_acpl(evals, 'black', cap = 1000, cap_action = 'replace'))
  black_col_b <- c(pgn$Black[[game_number]],
                   'inaccuracies', 'mistakes', 'blunders',
                   'Average cewntipawn loss')
  black_caption <- make_table(black_col_a, black_col_b)

  # Generate stat tables
   # White
  tw <- ggplot2::ggplot(data = data.frame(x = 0, y = 0),
                        ggplot2::aes(x = .data$x, y = .data$y)) +
    ggfittext::geom_fit_text(
      label = white_caption,
      family = 'mono',
      min.size = 0,
      hjust = 0
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(
      fill  = grDevices::rgb(237, 235, 233, maxColorValue = 255),
      color = grDevices::rgb(237, 235, 233, maxColorValue = 255)))
  tb <- ggplot2::ggplot(data = data.frame(x = 0, y = 0),
                        ggplot2::aes(x = .data$x, y = .data$y)) +
    ggfittext::geom_fit_text(
      label = black_caption,
      family = 'mono',
      min.size = 0,
      hjust = 0
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(
      fill  = grDevices::rgb(237, 235, 233, maxColorValue = 255),
      color = grDevices::rgb(237, 235, 233, maxColorValue = 255)))
# Output the result
  patchwork::wrap_plots(
    list(p1, tw, p2, tb),
    design = 'AAAAAABB
              CCCCCCDD'
  )
}
