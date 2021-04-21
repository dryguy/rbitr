test_that('evaluate_game evaluates a game', {
  movetext <- '1. e4 g5 2. Nc3 f5 3. Qh5# 1-0'
  engine <- bigchess::uci_engine('//stockfish_13_win_x64_bmi2.exe')
  analysis <- evaluate_game(movetext, engine, n_pv = 1, depth = 1)
  expect_identical(class(analysis), 'list')
  expect_identical(analysis[[1]][2], 'readyok')
  expect_identical(analysis[[6]][5], 'bestmove (none)')
  bigchess::uci_quit(engine)
})
