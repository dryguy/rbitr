dataframe <- data.frame(
  x = 1:4,
  y = c(1, -1, 1, -1)
)
p1 <- plot_2_color_area(dataframe, 'x', 'y')
test_that("plot_2_color_area plots 2 color area", {
  expect_equal(class(p1), c('gg', 'ggplot'))
})
