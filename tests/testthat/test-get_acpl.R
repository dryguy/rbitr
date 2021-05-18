scores <- c(15, 5, 29, -94, 67, 76, 154, -31, 1000)
test_that('get_acpl gets the average centipawn loss', {
  expect_equal(get_acpl(scores, 'white'), 79)
})

