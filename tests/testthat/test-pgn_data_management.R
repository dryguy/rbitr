source_movetext <- '1. d4 { [%clk 0:15:00] } 1... d5 { [%clk 0:15:00] } 2. Nc3 { [%clk 0:14:48] } 2... Bf5 { [%clk 0:14:55] }  0-1 '
test_that('get_clocks can get clock data', {
  test <- get_clocks(source_movetext)
  expect_identical(class(test), 'numeric')
})

time_control <- c('?', '-', '40/9000', '300', '4500+60', '*180')
test_that('get_increments can get increments', {
  test <- get_increments(time_control)
  expect_identical(class(test), 'integer')
  expect_equal(test, c(0, 0, 0, 0, 60, 0))
})
