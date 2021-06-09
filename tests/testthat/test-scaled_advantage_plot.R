test_that('scaled_advantage_plot returns a ggplot', {
  scores <- c(15, 5, 29, -94, 67, 76, 154, -31, 1000, 1000)
  p1 <- scaled_advantage_plot(scores)
  expect_equal(class(p1), c('gg', 'ggplot'))
  p1 <- scaled_advantage_plot(scores, style = 'infographic')
  expect_equal(class(p1), c('gg', 'ggplot'))
})
