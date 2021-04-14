pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'fools_mate.pgn'
)
pgn <- read_pgn(pgn_path)
fools_mate <- data.frame(
  Event          = 'Casual Game',
  Site           = '221B Baker Street',
  Date           = '1887',
  Round          = '?',
  White          = 'Sherlock Holmes',
  Black          = 'John H. Watson',
  Result         = '1-0',
  Movetext       = '1. e4 g5 2. Nc3 f5 3. Qh5#',
  SourceMovetext = ' 1. e4 g5 2. Nc3 f5 3. Qh5# ',
  NMoves         = 3
)
test_that('read_pgn reads pgns', {
  expect_identical(class(pgn), 'data.frame')
  expect_identical(pgn, fools_mate)
  expect_error(read_pgn('D:/no_file'), 'File not found.')
})
