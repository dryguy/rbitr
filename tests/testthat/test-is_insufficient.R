test_that("is_insufficient correctly identifies insufficient material", {
  # Create a board with only kings
  board <- matrix(rep("", 64), nrow = 8)
  board[c(1, 64)] <- c("K", "k")
  expect_true(is_insufficient(board))

  # Add a bishop
  board[2] <- "B"
  expect_true(is_insufficient(board))

  # Add a knight
  board[3] <- "N"
  expect_false(is_insufficient(board))

  # Remove the knight and add bishops on same color squares
  board[3] <- ""
  board[11] <- "B"
  expect_true(is_insufficient(board))

  # Add a bishop on a different color square
  board[10] <- "B"
  expect_false(is_insufficient(board))
})

test_that("is_insufficient correctly identifies sufficient material", {
  # Create a board with a pawn
  board <- matrix(rep("", 64), nrow = 8)
  board[c(1, 64, 2)] <- c("K", "k", "P")
  expect_false(is_insufficient(board))

  # Replace the pawn with a rook
  board[2] <- "R"
  expect_false(is_insufficient(board))

  # Replace the rook with a queen
  board[2] <- "Q"
  expect_false(is_insufficient(board))
})
