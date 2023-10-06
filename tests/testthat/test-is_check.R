board <- fen_to_board()$board

testthat::test_that('is_check validates input', {
  expect_error(is_check(board, 'red'))
  board <- matrix(1:9, nrow = 3)
  expect_error(is_check(board, 'white'))
})

testthat::test_that('is_check detects no checks', {
  expect_false(is_check(board, 'white'))
  expect_false(is_check(board, 'black'))
})

testthat::test_that('is_check detects pawn checks', {
  board <- matrix(rep('', 64), nrow = 8)
  board[5, 5] <- 'k'
  board[4, 4] <- 'P'
  expect_true(is_check(board, 'black'))
  board <- matrix(rep('', 64), nrow = 8)
  board[1, 8] <- 'K'
  board[2, 7] <- 'p'
  expect_true(is_check(board, 'white'))
})

testthat::test_that('is_check detects knight checks', {
  board <- matrix(rep('', 64), nrow = 8)
  board[5, 5] <- 'k'
  board[3, 4] <- 'N'
  expect_true(is_check(board, 'black'))
  board <- matrix(rep('', 64), nrow = 8)
  board[1, 8] <- 'K'
  board[3, 7] <- 'n'
  expect_true(is_check(board, 'white'))
})

testthat::test_that('is_check detects bishop checks', {
  board <- matrix(rep('', 64), nrow = 8)
  board[5, 5] <- 'k'
  board[3, 3] <- 'B'
  expect_true(is_check(board, 'black'))
  board <- matrix(rep('', 64), nrow = 8)
  board[1, 8] <- 'K'
  board[8, 1] <- 'b'
  expect_true(is_check(board, 'white'))
  board[6, 3] <- 'P'
  expect_false(is_check(board, 'white'))
})

testthat::test_that('is_check detects rook checks', {
  board <- matrix(rep('', 64), nrow = 8)
  board[5, 5] <- 'k'
  board[1, 5] <- 'R'
  expect_true(is_check(board, 'black'))
  board <- matrix(rep('', 64), nrow = 8)
  board[1, 8] <- 'K'
  board[8, 8] <- 'r'
  expect_true(is_check(board, 'white'))
  board[5, 8] <- 'P'
  expect_false(is_check(board, 'white'))
})

testthat::test_that('is_check detects queen checks', {
  board <- matrix(rep('', 64), nrow = 8)
  board[5, 5] <- 'k'
  board[1, 5] <- 'Q'
  expect_true(is_check(board, 'black'))
  board <- matrix(rep('', 64), nrow = 8)
  board[1, 8] <- 'K'
  board[8, 8] <- 'q'
  expect_true(is_check(board, 'white'))
  board[5, 8] <- 'P'
  expect_false(is_check(board, 'white'))
  board <- matrix(rep('', 64), nrow = 8)
  board[5, 5] <- 'k'
  board[3, 3] <- 'Q'
  expect_true(is_check(board, 'black'))
  board <- matrix(rep('', 64), nrow = 8)
  board[1, 8] <- 'K'
  board[8, 1] <- 'q'
  expect_true(is_check(board, 'white'))
  board[6, 3] <- 'P'
  expect_false(is_check(board, 'white'))
})
