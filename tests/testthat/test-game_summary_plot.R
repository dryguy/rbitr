pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata'
)
engine_paths <- c(
  '//stockfish_20090216_x64_bmi2.exe',
  '//stockfish_13_win_x64_bmi2.exe',
  '//stockfish_14_x64_avx2.exe',
  '//stockfish_14.1_win_x64_avx2.exe',
  '//stockfish_15_x64_avx2.exe')
for (engine_path in engine_paths) {
  game_number <- 1
  test_that('game_summary_plot is a ggplot', {
    pgn_path <- file.path(pgn_path, 'short_game.pgn')
    p1 <- game_summary_plot(pgn_path, game_number, engine_path)
    expect_identical(class(p1), c('patchwork', 'gg', 'ggplot'))
    p1 <- game_summary_plot(pgn_path, game_number, engine_path,
                            style = 'infographic')
    expect_identical(class(p1), c('patchwork', 'gg', 'ggplot'))
  })
  test_that('game_summary_plot works without saved data', {
    pgn_path <- file.path(pgn_path, 'shortest_game.pgn')
    p1 <- game_summary_plot(pgn_path, game_number, engine_path,
                            limiter = 'nodes', limit = 10)
    expect_identical(class(p1), c('patchwork', 'gg', 'ggplot'))
  })
  test_that('game_summary_plot returns error if no engine or saved analysis', {
    pgn_path <- file.path(pgn_path, 'shortest_game.pgn')
    expect_error(game_summary_plot(pgn_path, game_number))
  })
}
