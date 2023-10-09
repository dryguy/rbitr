test_that("update_board works", {
  board <- fen_to_board()
  board <- update_board('e2e4', board)
  fen <- board_to_fen(board)
  expect_equal(fen,
               'rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1')
  board <- update_board('c7c5', board)
  fen <- board_to_fen(board)
  expect_equal(fen,
               'rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2')
  board <- update_board('g1f3', board)
  fen <- board_to_fen(board)
  expect_equal(fen,
               'rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2')
})
