test_that('lichess_plot is a ggplot', {
  pgn_path <- file.path(
    system.file(package = 'rbitr'),
    'extdata',
    'short_game.pgn'
  )
  game_number <- 1
  engine_path <- '//stockfish_13_win_x64_bmi2.exe'
  p1 <- lichess_plot(pgn_path, game_number = 1, engine_path)
  expect_equal(class(p1), c('patchwork', 'gg', 'ggplot'))
})
