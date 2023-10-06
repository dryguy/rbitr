test_that("print_board works correctly", {
  board <- fen_to_board()$board

  # Capture the console output to prevent actual printing during test
  capture_print_board <- function(board, pov) {
    capture.output(print_board(board, pov))
  }


  # Prints correctly from white's point of view
  expect_equal(capture_print_board(board, 'white'),
               capture.output(print(board[8:1, ])))

  # Prints correctly from black's point of view
  expect_equal(capture_print_board(board, 'black'),
               capture.output(print(apply(board, 1, rev))))

  # Invalid pov
  expect_error(print_board(board, 'red'))

  # Invalid board (not a matrix)
  expect_error(print_board(c(1, 2, 3), 'white'))

  # Invalid board (not 8x8)
  expect_error(print_board(matrix(1:9, nrow = 3), 'white'))
})
