time_control <- c('?', '-', '40/9000', '300', '4500+60', '*180')
test_that('get_increments gets increments', {
  test <- get_increments(time_control)
  expect_identical(class(test), 'integer')
  expect_equal(test, c(0, 0, 0, 0, 60, 0))
})
