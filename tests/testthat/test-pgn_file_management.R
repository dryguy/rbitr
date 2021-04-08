test_that("get_pgn_tags gets pgn tags", {
  test <- get_pgn_tags('/extdata/fools_mate.pgn')
  expect_identical(
    test,
    c('Event', 'Site', 'Date', 'Round', 'White', 'Black', 'Result')
  )
})
