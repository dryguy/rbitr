pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata'
)
engine_path <- '//stockfish_13_win_x64_bmi2.exe'
game_number <- 1
test_that('lichess_plot is a ggplot', {
  pgn_path <- file.path(pgn_path, 'short_game.pgn')
  p1 <- lichess_plot(pgn_path, game_number, engine_path)
  expect_identical(class(p1), c('patchwork', 'gg', 'ggplot'))
})
test_that('lichess_plot works without saved data', {
  pgn_path <- file.path(pgn_path, 'shortest_game.pgn')
  p1 <- lichess_plot(pgn_path, game_number, engine_path, nodes = 10)
  expect_identical(class(p1), c('patchwork', 'gg', 'ggplot'))
})
