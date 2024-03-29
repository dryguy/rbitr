test_that("board_to_fen works", {
  board <- fen_to_board()
  fen <- board_to_fen(board)
  expect_equal('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1', fen)

  board <- fen_to_board('rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2')
  fen <- board_to_fen(board)
  expect_equal('rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2', fen)
})
