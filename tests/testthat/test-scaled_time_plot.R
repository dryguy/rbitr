test_that('scaled_time_plot is a ggplot', {
  white_move_times <- c(4, 10, 5, 10)
  black_move_times <- c(3, 4, 7)
  p1 <- scaled_time_plot(white_move_times, black_move_times)
  expect_equal(class(p1), c('gg', 'ggplot'))
  p1 <- scaled_time_plot(white_move_times, black_move_times,
                         style = 'infographic')
  expect_equal(class(p1), c('gg', 'ggplot'))
})
