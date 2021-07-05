pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata'
)
engine_path <- '//stockfish_14_x64_avx2.exe'
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
  p1 <- game_summary_plot(pgn_path, game_number, engine_path, nodes = 10)
  expect_identical(class(p1), c('patchwork', 'gg', 'ggplot'))
})
test_that('game_summary_plot returns error if no engine or saved analysis', {
  pgn_path <- file.path(pgn_path, 'shortest_game.pgn')
  expect_error(game_summary_plot(pgn_path, game_number))
})
