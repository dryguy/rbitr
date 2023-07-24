test_that('get_moves gets moves', {
  expect_identical(
    get_moves(c('1. e4 e5', '1. h3 h6')),
    list(c('e2e4', 'e7e5'), c('h2h3', 'h7h6'))
  )
})

test_that('get_moves handles empty movetext', {
  movetext <- c('1. e4 e5', '', '1. h3 h6', '')
  expect_identical(
    get_moves(movetext),
    list(c('e2e4', 'e7e5'), c(''), c('h2h3', 'h7h6'), c(''))
  )
})
