position <- 'e2e4 g7g5 b1c3'
engine <- bigchess::uci_engine('//stockfish_13_win_x64_bmi2.exe')
test_that('evaluate_positions returns a character vector', {
  test <- evaluate_positions(position, engine, n_pv = 1, depth = 1)
  expect_identical(class(test), 'list')
  expect_identical(test[[1]][2], 'readyok')
  test <- evaluate_positions( '', engine, n_pv = 2, depth = 1)
  expect_identical(class(test), 'list')
  expect_identical(test[[1]][2], 'readyok')
})
