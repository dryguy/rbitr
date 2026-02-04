test_that('time_plot is a ggplot', {
  white_move_times <- c(4, 10, 5, 10)
  black_move_times <- c(3, 4, 7)
  p1 <- time_plot(white_move_times, black_move_times)
  testthat::expect_true(all(c("gg", "ggplot") %in% class(p1)))
  p1 <- time_plot(white_move_times, black_move_times, scaling = 'none')
  testthat::expect_true(all(c("gg", "ggplot") %in% class(p1)))
  p1 <- time_plot(white_move_times, black_move_times,
                  style = 'infographic')
  testthat::expect_true(all(c("gg", "ggplot") %in% class(p1)))
})
