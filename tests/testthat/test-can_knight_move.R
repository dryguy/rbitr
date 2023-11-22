test_that("can_knight_move returns correct values", {
  # Test 1: Starting position, white knight at (1, 2)
  board <- fen_to_board() # Starting position
  coordinates <- c(1, 2)  # Coordinates of the white knight in the starting position
  expect_true(can_knight_move(board, coordinates))

  # Test 2: Knight surrounded by empty squares
  board$board <- matrix("", nrow = 8, ncol = 8)
  board$board[4, 4] <- "N"
  board$board[1, 1] <- "k"
  board$board[8, 8] <- "K"
  coordinates <- c(4, 4)
  expect_true(can_knight_move(board, coordinates))

  # Test 3: Knight surrounded by friendly pieces
  board$board <- matrix("P", nrow = 8, ncol = 8)
  board$board[4, 4] <- "N"
  coordinates <- c(4, 4)
  expect_false(can_knight_move(board, coordinates))

  # Test 4: Knight surrounded by enemy pieces
  board$board <- matrix("p", nrow = 8, ncol = 8)
  board$board[4, 4] <- "N"
  board$board[1, 1] <- "k"
  board$board[8, 8] <- "K"
  coordinates <- c(4, 4)
  expect_true(can_knight_move(board, coordinates))
})
