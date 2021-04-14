engine <- bigchess::uci_engine('//stockfish_13_win_x64_bmi2.exe')
test_that('search_move searches a move', {
  search_log <- search_move(position = '', move = 'h2h3', engine = engine, depth = 1)
  expect_identical(search_log[[2]], 'readyok')
})
