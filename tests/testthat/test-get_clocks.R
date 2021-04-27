movetext <- c(
  '1. d4 {[%clk 0:15:00]} d5 {[%clk 0:15:00]} 2. Nc3 {[%clk 0:14:48]}',
  '1. e4 {[%clk 0:15:00]} e5 {[%clk 0:15:00]} 2. Nf3 {[%clk 0:14:38]}'
)
test_that('get_clocks gets clocks', {
  clocks <- get_clocks(movetext)
  expect_identical(class(clocks), 'list')
  expect_identical(class(clocks[[1]]), 'numeric')
  expect_equal(clocks, list(c(900, 900, 888), c(900, 900, 878)))
})
