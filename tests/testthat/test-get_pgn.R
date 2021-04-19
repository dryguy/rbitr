pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata'
)
fools_mate <- data.frame(
  Event          = 'Casual Game',
  Site           = '221B Baker Street',
  Date           = '1887',
  Round          = '?',
  White          = 'Sherlock Holmes',
  Black          = 'John H. Watson',
  Result         = '1-0',
  Movetext       = '1. e4 g5 2. Nc3 f5 3. Qh5# 1-0'
)
test_that('get pgn gets an export format pgn', {
  pgn <- get_pgn(file.path(pgn_path, 'fools_mate.pgn'))
  expect_identical(pgn, fools_mate)
})
test_that('get pgn gets an import format pgn', {
  pgn <- get_pgn(file.path(pgn_path, 'pathological_spacing.pgn'))
  expect_identical(pgn, fools_mate)
})
test_that('get pgn gets an empty pgn', {
  pgn <- get_pgn(file.path(pgn_path, 'empty.pgn'))
  expect_identical(pgn, data.frame())
})
