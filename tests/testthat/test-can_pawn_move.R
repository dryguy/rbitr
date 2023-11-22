test_that("can_pawn_move returns correct values", {
  # Starting position, white pawn at (2, 1)
  board <- fen_to_board() # Starting position
  coordinates <- c(2, 1)  # Coordinates of the white pawn in the starting position
  expect_true(can_pawn_move(board, coordinates))

  # Pawn surrounded by empty squares
  board$board <- matrix("", nrow = 8, ncol = 8)
  board$board[4, 4] <- "P"
  board$board[1, 1] <- "k"
  board$board[8, 8] <- "K"
  coordinates <- c(4, 4)
  expect_true(can_pawn_move(board, coordinates))

  # Pawn surrounded by friendly pieces
  board$board <- matrix("P", nrow = 8, ncol = 8)
  board$board[4, 4] <- "P"
  coordinates <- c(4, 4)
  expect_false(can_pawn_move(board, coordinates))

  # Pawn surrounded by enemy pieces
  board$board <- matrix("p", nrow = 8, ncol = 8)
  board$board[4, 4] <- "P"
  board$board[1, 1] <- "k"
  board$board[8, 8] <- "K"
  coordinates <- c(4, 4)
  expect_true(can_pawn_move(board, coordinates))

  # En passant
  fen <- "8/8/2k1p3/4Pp2/8/8/3K4/8 w - f6 0 30"
  board <- fen_to_board(fen)
  coordinates <- c(5, 5)
  expect_true(can_pawn_move(board, coordinates))
  fen <- "8/3k4/4p3/4Pp2/8/2K5/8/8 w - - 2 31"
  board <- fen_to_board(fen)
  expect_false(can_pawn_move(board, coordinates))
})
