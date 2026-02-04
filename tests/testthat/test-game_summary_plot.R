# Path for pgn files used in unit tests
pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'test'
)

# Load the locations of the chess engines that will be tested
engines_file_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'engine_paths.R'
)
source(engines_file_path)

for (engine_path in engine_paths) {
  game_number <- 1
  test_that('game_summary_plot is a ggplot', {
    pgn_path <- file.path(pgn_path, 'short_game.pgn')
    p1 <- game_summary_plot(pgn_path, game_number, engine_path)
    testthat::expect_true(all(c("patchwork", "ggplot") %in% class(p1)))
    p1 <- game_summary_plot(pgn_path, game_number, engine_path,
                            style = 'infographic')
    testthat::expect_true(all(c("patchwork", "ggplot") %in% class(p1)))
  })
  test_that('game_summary_plot works without saved data', {
    pgn_path <- file.path(pgn_path, 'shortest_game.pgn')
    p1 <- game_summary_plot(pgn_path, game_number, engine_path,
                            limiter = 'nodes', limit = 10)
    testthat::expect_true(all(c("patchwork", "ggplot") %in% class(p1)))
  })
  test_that('game_summary_plot returns error if no engine or saved analysis', {
    pgn_path <- file.path(pgn_path, 'shortest_game.pgn')
    expect_error(game_summary_plot(pgn_path, game_number))
  })
}
