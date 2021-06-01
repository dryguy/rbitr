test_that('get_move_times calculates move times', {
  clocks <- c(900, 900, 888, 878, 878, 858)
  move_times <- get_move_times(clocks, 8, 'white')
  expect_identical(class(move_times), 'numeric')
  expect_equal(move_times, c(20, 18))
  move_times <- get_move_times(clocks, 8, 'black')
  expect_identical(class(move_times), 'numeric')
  expect_equal(move_times, c(30, 28))
  expect_error(get_move_times(clocks, 8, 'wite'))
  clocks <- c(900, 900, 910, 878)
  expect_identical(get_move_times(clocks, 8, 'white'), NA_real_)
  expect_identical(
    get_move_times(clocks, 8, 'white', negative_time = 'no action'),
    -2
  )
  expect_identical(get_move_times(numeric(0), 8, 'white'), numeric(0))
})
