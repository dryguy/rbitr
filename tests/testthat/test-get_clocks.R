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
