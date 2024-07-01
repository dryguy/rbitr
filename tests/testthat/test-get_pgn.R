pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'test'
)
fools_mate <- tibble::tibble(
  Event    = 'Casual Game',
  Site     = '221B Baker Street',
  Date     = '1887.04.01',
  Round    = '?',
  White    = 'Sherlock Holmes',
  Black    = 'John H. Watson',
  Result   = '1-0',
  Movetext = '1. e4 g5 2. Nc3 f5 3. Qh5# 1-0'
)
test_that('get_pgn gets an export format pgn', {
  pgn_path <- file.path(pgn_path, 'export_format.pgn')
  pgn <- get_pgn(pgn_path)
  expect_identical(pgn[1, ], fools_mate)
})
test_that('get_pgn gets an import format pgn', {
  pgn_path <- file.path(pgn_path, 'import_format.pgn')
  pgn <- get_pgn(pgn_path)
  expect_identical(pgn, fools_mate)
})
test_that('get_pgn gets an empty pgn', {
  pgn_path <- file.path(pgn_path, 'empty.pgn')
  pgn <- get_pgn(pgn_path)
  expect_identical(pgn, tibble::tibble())
})
test_that('get_pgn handles missing movetext', {
  pgn_path <- file.path(pgn_path, 'no_movetext.pgn')
  pgn <- get_pgn(pgn_path)
  expect_identical(pgn$Movetext, NA)
})
test_that('get_pgn handles line breaks in tag pairs', {
  pgn_path <- file.path(pgn_path, 'line_break_in_tag_pair.pgn')
  pgn <- get_pgn(pgn_path)
  expect_identical(pgn, fools_mate)
})
