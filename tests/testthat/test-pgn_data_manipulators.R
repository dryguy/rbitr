source_movetext <- c(
  '1. d4 {[%clk 0:15:00]} d5 {[%clk 0:15:00]} 2. Nc3 {[%clk 0:14:48]}',
  '1. e4 {[%clk 0:15:00]} e5 {[%clk 0:15:00]} 2. Nf3 {[%clk 0:14:38]}'
)
test_that('get_clocks gets clocks', {
  test <- get_clocks(source_movetext)
  expect_identical(class(test), 'list')
  expect_identical(class(test[[1]]), 'numeric')
  expect_equal(test, list(c(900, 900, 888), c(900, 900, 878)))
})

time_control <- c('?', '-', '40/9000', '300', '4500+60', '*180')
test_that('get_increments gets increments', {
  test <- get_increments(time_control)
  expect_identical(class(test), 'integer')
  expect_equal(test, c(0, 0, 0, 0, 60, 0))
})

clock <- c(900, 900, 888, 878, 878, 858)
test_that ('calculate_move_times calculates move times', {
  test <- calculate_move_times(clock, 8, 'white')
  expect_identical(class(test), 'numeric')
  expect_equal(test, c(20, 18))
  test <- calculate_move_times(clock, 8, 'black')
  expect_equal(test, c(30, 28))
  expect_error(
    calculate_move_times(clock, increment, 'wite'),
    'Allowed colors are "white" or "black".'
  )
})
