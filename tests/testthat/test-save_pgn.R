pgn_path <- tempfile()
#  file.path(
#  system.file(package = 'rbitr'),
#  'extdata',
#  'test',
#  'save_pgn_test.pgn'
#)
pgn <- tibble::tibble(
  Event    = 'Casual Game',
  Site     = '221B Baker Street',
  Date     = '1887.04.01',
  Round    = '?',
  White    = 'Sherlock Holmes',
  Black    = 'John H. Watson',
  Result   = '1-0',
  Movetext = '1. e4 g5 2. Nc3 f5 3. Qh5# 1-0'
)
test_that('save_pgn saves a PGN', {
  save_pgn(pgn, pgn_path)
  saved_example <- get_pgn(pgn_path)
  expect_identical(pgn, saved_example)
  file.remove(pgn_path)
})
test_that('save_pgn appends games to a PGN', {
  save_pgn(pgn, pgn_path)
  save_pgn(pgn, pgn_path, append = TRUE)
  saved_example <- get_pgn(pgn_path)
  expect_identical(rbind(pgn, pgn), saved_example)
  file.remove(pgn_path)
})
test_that('save_pgn adds missing seven tag roster tags', {
  missing_str <- pgn[, -2]
  save_pgn(missing_str, pgn_path)
  saved_example <- get_pgn(pgn_path)
  missing_str$Site <- '?'
  tag_names <- c('Event', 'Site', 'Date', 'Round', 'White', 'Black',
                 'Result', 'Movetext')
  missing_str <- missing_str[, tag_names]
  expect_identical(missing_str, saved_example)
  file.remove(pgn_path)
})
test_that('save_pgn replaces NA with ?', {
  na_pgn <- pgn
  na_pgn$Date <- NA
  save_pgn(na_pgn, pgn_path)
  saved_example <- get_pgn(pgn_path)
  na_pgn$Date <- '?'
  expect_identical(na_pgn, saved_example)
  file.remove(pgn_path)
})
test_that('save_pgn replaces invalid tag names' , {
  tag_names <- names(pgn)
  tag_names[2] <- 'Si.te'
  tag_names[3] <- '-Date'
  tag_names[4] <- '9Round'
  tag_names[5] <- '_White'
  tag_names[6] <- 'Si-te'
  illegal_pgn <- pgn
  names(illegal_pgn) <- tag_names
  save_pgn(illegal_pgn, pgn_path)
  illegal_pgn <- get_pgn(pgn_path)
  expect_identical(names(illegal_pgn), c('Event', 'Site', 'Date', 'Round',
    'White', 'Black', 'Result', 'Si_te1', 'Si_te2', 'X9Round', 'X_Date',
    'X_White', 'Movetext'))
  file.remove(pgn_path)
})
if (file.exists(pgn_path)) {
  file.remove(pgn_path)
}
