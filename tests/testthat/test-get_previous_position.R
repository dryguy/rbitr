movetext <- 'e2e4 g7g5 b1c3 f7f5 d1h5'
previous_position <- get_previous_position(movetext)
test_that('get_previous_position gets the previous position', {
  expect_identical(previous_position, 'e2e4 g7g5 b1c3 f7f5')
  expect_error(get_previous_position(list('e2e4', 'e2e4 e7e5')))
})
