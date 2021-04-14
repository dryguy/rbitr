games <- list(c('', 'e2e4', 'e2e4 g7g5', 'e2e4 g7g5 b1c3',
                'e2e4 g7g5 b1c3 f7f5', 'e2e4 g7g5 b1c3 f7f5 d1h5'))
engine <- bigchess::uci_engine('//stockfish_13_win_x64_bmi2.exe')
test <- evaluate_games(games, engine, n_pv = 1, depth = 1)
test_that('evaluate_games evaluates games', {
  expect_identical(test[[1]][[1]][2], 'readyok')
  expect_identical(test[[1]][[6]][5], 'bestmove (none)')
})
