pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'fools_mate.pgn'
)
tags <- get_pgn_tags(pgn_path)
pgn <- read_pgn(pgn_path)
fools_mate <- data.frame(
  Event          = '3D Chess',
  Site           = 'USS Enterprise (NCC-1701)',
  Date           = '1312.4',
  Round          = '?',
  White          = 'Captain James T. Kirk',
  Black          = 'Lieutenant Commander Spock',
  Result         = '1-0',
  Movetext       = '1. e4 g5 2. Nc3 f5 3. Qh5#',
  SourceMovetext = ' 1. e4 g5 2. Nc3 f5 3. Qh5# ',
  NMoves         = 3
)

test_that("get_pgn_tags gets pgn tags", {
  expect_identical(
    tags,
    c('Event', 'Site', 'Date', 'Round', 'White', 'Black', 'Result')
  )
})

test_that ('read_pgn reads pgns', {
  expect_identical(class(pgn), 'data.frame')
  expect_identical(pgn, fools_mate)
  expect_error(read_pgn('D:/no_file'), 'File not found.')
})
