engine <- bigchess::uci_engine('//stockfish_13_win_x64_bmi2.exe')
moves <- c("e2e4", "g7g5", "b1c3", "f7f5", "d1h5")
positions <- c( "", "e2e4", "e2e4 g7g5", "e2e4 g7g5 b1c3",
                "e2e4 g7g5 b1c3 f7f5")
test_that('search_moves searches moves', {
  search_log <- search_moves(positions, moves, engine, depth = 1)
  expect_identical(search_log[[1]][2], 'readyok')
  expect_identical(search_log[[5]][5], 'bestmove d1h5')
})

positions <- c('', 'c2c3')
moves <- c('c2c3', 'd7d5')
search_log <- search_moves(positions, moves, engine, depth = 1)
