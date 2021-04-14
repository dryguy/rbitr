pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'fools_mate.pgn'
)
tags <- get_pgn_tags(pgn_path)
test_that("get_pgn_tags gets pgn tags", {
  expect_identical(
    tags,
    c('Event', 'Site', 'Date', 'Round', 'White', 'Black', 'Result')
  )
})
