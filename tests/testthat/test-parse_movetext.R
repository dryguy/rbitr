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
  expect_equal(evals, list(c(5, 29, -94)))
})
test_that('get_evals gets evals when last position is mate', {
  movetext <- c('4... Nc5 { [%eval #1] [%clk 0:05:10] } 5. Qxf7# { [%clk 0:05:03] } 1-0')
  expect_equal(get_evals(movetext, mate_value = 5000, mate0 = TRUE), list(c(5000, 5000)))
  movetext <- c('4. Nc5 { [%eval #-1] [%clk 0:05:10] } 4... Qxf7# { [%clk 0:05:03] } 1-0')
  expect_equal(get_evals(movetext, mate_value = 5000, mate0 = TRUE), list(c(-5000, -5000)))
  movetext <- c('4... Nc5+ { [%eval #1] [%clk 0:05:10] } 5. Qxf7# { [%clk 0:05:03] } 1-0')
  expect_equal(get_evals(movetext, mate_value = 5000, mate0 = TRUE), list(c(5000, 5000)))
  movetext <- c('4. Nc5+ { [%eval #-1] [%clk 0:05:10] } 4... Qxf7# { [%clk 0:05:03] } 1-0')
  expect_equal(get_evals(movetext, mate_value = 5000, mate0 = TRUE), list(c(-5000, -5000)))
})
