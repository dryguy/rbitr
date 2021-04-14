clock <- c(900, 900, 888, 878, 878, 858)
test_that('calculate_move_times calculates move times', {
  test <- calculate_move_times(clock, 8, 'white')
  expect_identical(class(test), 'numeric')
  expect_equal(test, c(20, 18))
  test <- calculate_move_times(clock, 8, 'black')
  expect_identical(class(test), 'numeric')
  expect_equal(test, c(30, 28))
  expect_error(
    calculate_move_times(clock, 8, 'wite'),
    'Allowed colors are "white" or "black".'
  )
})
