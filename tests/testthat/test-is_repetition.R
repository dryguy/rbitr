test_that("is_repetition works", {
  # Repeat positions by moving knights back and forth
  boards <- list(fen_to_board()) # starting position
  boards[[2]] <- update_board('g1f3', boards[[1]])
  boards[[3]] <- update_board('g8f6', boards[[2]])
  boards[[4]] <- update_board('f3g1', boards[[3]])
  boards[[5]] <- update_board('f6g8', boards[[4]]) # back to starting position
  expect_equal(is_repetition(boards, 3), FALSE)
  boards[[6]] <- update_board('g1f3', boards[[5]])
  boards[[7]] <- update_board('g8f6', boards[[6]])
  boards[[8]] <- update_board('f3g1', boards[[7]])
  boards[[9]] <- update_board('f6g8', boards[[8]]) # back to starting position
  expect_equal(is_repetition(boards, 3), TRUE)
})
