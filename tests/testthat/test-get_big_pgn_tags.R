pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'fools_mate.pgn'
)
test_that("get_big_pgn_tags gets pgn tags", {
  tags <- get_big_pgn_tags(pgn_path, silent = TRUE)
  expect_identical(
    tags,
    c('Event', 'Site', 'Date', 'Round', 'White', 'Black', 'Result')
  )
})
