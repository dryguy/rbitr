test_that("king_can_move works", {
  # King is blocked by own pieces and edge of board
  board <- fen_to_board()
  expect_false(can_king_move(board))

  # King has a valid move
  board <- fen_to_board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2")
  expect_true(can_king_move(board))

  # King is blocked by checks
  board <- fen_to_board("rnb1k1n1/ppp2pp1/4p1r1/2bq3p/P4K2/8/1PPP1PPP/RNBQ1BNR w q - 1 8")
  expect_false(can_king_move(board))

  # King is in stalemate
  board <- fen_to_board("5bnr/4p1pq/4Qpkr/7p/2P4P/8/PP1PPPP1/RNB1KBNR b KQ - 2 10")
  expect_false(king_can_move(board))

  # King is in checkmate
  board <- fen_to_board("rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3")
  expect_false(can_king_move(board))
})
