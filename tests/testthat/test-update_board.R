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

test_that("promotions are to correct color", {
  fen <- "rnbqkbnr/ppppp1pp/8/8/8/5N2/PPp1BPPP/RNBQ1RK1 b kq - 1 5"
  board <- fen_to_board(fen)
  board <- update_board('c2d1N', board)
  expect_equal(board_to_fen(board),
               'rnbqkbnr/ppppp1pp/8/8/8/5N2/PP2BPPP/RNBn1RK1 w kq - 0 6')
})

test_that("update_board updates castling rights", {
  fen <- "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
  board <- fen_to_board(fen)
  board <- update_board('e1e2', board)
  expect_equal(board_to_fen(board),
               'rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPPKPPP/RNBQ1BNR b kq - 1 2')
})
