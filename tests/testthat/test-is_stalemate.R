test_that("is_stalemate correctly identifies stalemate", {
  # Stalemate
  fen <- "8/8/8/2p2p1p/2P2P1k/4pP1P/4P1KP/5BNR w - - 0 19" # Both players in stalemate
  board <- fen_to_board(fen)
  expect_true(is_stalemate(board))
  expect_true(is_stalemate(board, "black"))

  # Checkmate (not a stalemate)
  fen <- "7k/5KQ1/8/8/8/8/8/8 b - - 0 1" # Checkmate
  board <- fen_to_board(fen)
  expect_false(is_stalemate(board))

  # Starting position (not a stalemate)
  fen <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" # Starting position
  board <- fen_to_board(fen)
  expect_false(is_stalemate(board))
})
