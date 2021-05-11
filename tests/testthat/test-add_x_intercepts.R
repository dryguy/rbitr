dataframe <- data.frame(
  x = 1:4,
  y = c(1, -1, 1, -1)
)
new_dataframe <- add_x_intercepts(dataframe, 'x', 'y')
test_that('add_x_intercepts adds x-intercepts', {
  expect_equal(new_dataframe$x, c(1, 1.5, 2, 2.5, 3, 3.5, 4))
})
