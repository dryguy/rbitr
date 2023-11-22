test_that("can_rook_move returns correct values", {
  # Starting position, white rook at (1, 1)
  board <- fen_to_board() # Starting position
  coordinates <- c(1, 1)  # Coordinates of the white rook in the starting position
  expect_false(can_rook_move(board, coordinates))

  # Rook surrounded by empty squares
  board$board <- matrix("", nrow = 8, ncol = 8)
  board$board[4, 4] <- "R"
  board$board[1, 1] <- "k"
  board$board[8, 8] <- "K"
  coordinates <- c(4, 4)
  expect_true(can_rook_move(board, coordinates))

  # Rook surrounded by friendly pieces
  board$board <- matrix("P", nrow = 8, ncol = 8)
  board$board[4, 4] <- "R"
  coordinates <- c(4, 4)
  expect_false(can_rook_move(board, coordinates))

  # Rook surrounded by enemy pieces
  board$board <- matrix("p", nrow = 8, ncol = 8)
  board$board[4, 4] <- "R"
  board$board[1, 1] <- "k"
  board$board[8, 8] <- "K"
  coordinates <- c(4, 4)
  expect_true(can_rook_move(board, coordinates))
})
