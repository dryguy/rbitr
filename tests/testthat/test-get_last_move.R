movetext <- 'e2e4 g7g5 b1c3 f7f5 d1h5'
test_that('get_last_move gets the last move', {
  last_move <- get_last_move(movetext)
  expect_identical(last_move, 'd1h5')
  expect_identical(get_last_move(list('')), '')
})
