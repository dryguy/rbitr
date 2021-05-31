scores <- c(15, 5, 29, -94, 67, 76, 154, -31, 1000)
test_that('get_acpl gets the average centipawn loss', {
  expect_equal(get_acpl(scores, 'white'), 79)
})
test_that('get_acpl works when cap_action = "exclude"', {
  expect_equal(
    get_acpl(scores, 'black', cap = 200, cap_action = 'exclude',
             to_move = 'black'),
    2
  )
})
test_that('get_acpl works when cap_action = "replace"', {
  expect_equal(
    get_acpl(scores, 'black', cap = 200, cap_action = 'replace'),
    123
  )
})
