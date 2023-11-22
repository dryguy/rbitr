test_that("is_checkmate correctly identifies checkmate", {
  # Generate boards from FEN strings
  # Assuming `fen_to_board` is a function that converts FEN strings to board objects

  # FEN string for a known checkmate position
  board <- fen_to_board("rnbqkbnr/ppppp1pp/8/8/4pP1q/8/PPPPP1PP/RNBQKBNR w KQkq - 0 3")
  expect_true(is_checkmate(board))
  expect_true(is_checkmate(board, color = "white"))

  # FEN string for a known non-checkmate position
  board <- fen_to_board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
  expect_false(is_checkmate(board))
  expect_false(is_checkmate(board, color = "white"))
})
