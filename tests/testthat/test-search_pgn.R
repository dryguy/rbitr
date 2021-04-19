pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'fools_mate.pgn'
)
pgn <- search_pgn(pgn_path, pattern = '1. e4 g5')
test_that('search_pgn searches pgns', {
  expect_identical(pgn$Site, '221B Baker Street')
})
pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'dr_who.pgn'
)
pgn <- search_pgn(pgn_path, pattern = '1. c3 d5')
test_that('search_pgn searches commented pgns', {
  expect_identical(pgn$White, 'DrNykterstein')
})
