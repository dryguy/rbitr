test_that("board_to_int works", {
  board <- fen_to_board()
  int_board <- board_to_int(board$board)
  expect_equal(int_board, unname(bigchess::position.start()))
})
