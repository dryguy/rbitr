test_that("can_queen_move returns FALSE if the queen has no legal moves", {
  board <- fen_to_board()
  expect_false(can_queen_move(board, c(1, 4)))
  expect_false(can_queen_move(board, c(8, 4), color = "black"))
})

test_that("can_queen_move returns TRUE if the queen can capture an enemy piece", {
  board <- fen_to_board()
  board$board[1, 3] <- "r"  # Place a black rook next to the white queen
  expect_true(can_queen_move(board, c(1, 4)))
})

test_that("can_queen_move returns TRUE when the queen is on the edge of the board and has legal moves", {
  fen <- 'rnbqkbnr/pppppppp/8/8/8/8/1PPPPPPP/QNB1KBNR w kq - 0 1'
  board <- fen_to_board(fen)
  expect_true(can_queen_move(board, c(1, 1)))
})

test_that("can_queen_move returns FALSE when the queen is on the edge of the board and has no legal moves", {
  fen <- 'qnb1kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1'
  board <- fen_to_board(fen)
  expect_false(can_queen_move(board, c(8, 8)))
})
