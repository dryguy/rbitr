test_that("can_bishop_move returns correct values", {
  # Starting position, white bishop at (1, 3)
  board <- fen_to_board() # Starting position
  coordinates <- c(1, 3)  # Coordinates of the white bishop in the starting position
  expect_false(can_bishop_move(board, coordinates))

  # Bishop surrounded by empty squares
  board$board <- matrix("", nrow = 8, ncol = 8)
  board$board[4, 4] <- "B"
  board$board[1, 1] <- "k"
  board$board[8, 8] <- "K"
  coordinates <- c(4, 4)
  expect_true(can_bishop_move(board, coordinates))

  # Test 3: Bishop surrounded by friendly pieces
  board$board <- matrix("P", nrow = 8, ncol = 8)
  board$board[4, 4] <- "B"
  coordinates <- c(4, 4)
  expect_false(can_bishop_move(board, coordinates))

  # Test 4: Bishop surrounded by enemy pieces
  board$board <- matrix("p", nrow = 8, ncol = 8)
  board$board[4, 4] <- "B"
  board$board[1, 1] <- "k"
  board$board[8, 8] <- "K"
  coordinates <- c(4, 4)
  expect_true(can_bishop_move(board, coordinates))
})
