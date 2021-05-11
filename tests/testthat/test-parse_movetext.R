movetext <- c(
  '1. d4 {[%clk 0:15:00]} d5 {[%clk 0:15:00]} 2. Nc3 {[%clk 0:14:48]}',
  '1. e4 {[%clk 0:15:00]} e5 {[%clk 0:15:00]} 2. Nf3 {[%clk 0:14:38]}'
)
test_that('parse_movetext parses movetext', {
  clocks <- parse_movetext(movetext, 'clk')
  expect_equal(clocks, list(c(900, 900, 888), c(900, 900, 878)))
})
test_that('get_clocks gets clocks', {
  clocks <- get_clocks(movetext)
  expect_equal(clocks, list(c(900, 900, 888), c(900, 900, 878)))
})
movetext <- c(
  '1. e4 { [%eval 0.05] } 1... Nf6 { [%eval 0.29] } 2. Bc4 { [%eval -0.94] }'
)
test_that('get_evals gets evals', {
  evals <- get_evals(movetext)
  expect_equal(evals, list(c(0.05, 0.29, -0.94)))
})