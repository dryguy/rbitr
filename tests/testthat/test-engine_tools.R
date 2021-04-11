position <- 'e2e4 g7g5 b1c3'
engine_path <- '../../../../stockfish_13_win_x64_bmi2.exe'
engine <- bigchess::uci_engine(engine_path)
test_that ('evaluate_position returns a character vector', {
  test <- evaluate_position(position, engine, n_pv = 1, depth = 1)
  expect_identical(class(test), 'character')
  expect_identical(test[2], 'readyok')
})
