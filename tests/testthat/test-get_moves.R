movetext <- '1. e4 g5 2. Nc3 f5 3. Qh5#'
test_that('get_moves gets moves', {
  test <- get_moves(movetext)
  expect_identical(test, list(c("e2e4", "g7g5", "b1c3", "f7f5", "d1h5")))
})
