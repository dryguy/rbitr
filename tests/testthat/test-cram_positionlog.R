engine_line <-
  "info depth 2 seldepth 2 multipv 2 score cp -154 nodes 119 nps 119000 tbhits 0 time 1 pv d7d6 d2d4"
test_that("parse_engine_line_cpp returns correct value for numeric tags", {
  expect_equal(parse_engine_line_cpp(engine_line, 'depth'), '2')
  expect_equal(parse_engine_line_cpp(engine_line, 'seldepth'), '2')
  expect_equal(parse_engine_line_cpp(engine_line, 'multipv'), '2')
  expect_equal(parse_engine_line_cpp(engine_line, 'nodes'), '119')
  expect_equal(parse_engine_line_cpp(engine_line, 'nps'), '119000')
  expect_equal(parse_engine_line_cpp(engine_line, 'tbhits'), '0')
  expect_equal(parse_engine_line_cpp(engine_line, 'time'), '1')
})

test_that("parse_engine_line_cpp works for multiple tags", {
  tag_names <- c('depth', 'multipv', 'score', 'pv')
  expect_equal(parse_engine_line_cpp(engine_line, tag_names),
               c('2', '2', 'cp -154', 'd7d6 d2d4'))
})

test_that("parse_engine_line_cpp returns correct value for move tags", {
  engine_line <- 'bestmove d2d4 ponder g8f6'
  expect_equal(parse_engine_line_cpp(engine_line, 'bestmove'), 'd2d4')
  expect_equal(parse_engine_line_cpp(engine_line, 'ponder'),   'g8f6')
  engine_line <-
    "info depth 2 seldepth 2 multipv 2 score cp -154 nodes 119 nps 119000 tbhits 0 time 1 pv d7d6 d2d4"
  expect_equal(parse_engine_line_cpp(engine_line, 'pv'), 'd7d6 d2d4')
  expect_equal(parse_engine_line_cpp(engine_line, 'currmove'), NA_character_)
  expect_equal(parse_engine_line_cpp(engine_line, 'refutation'), NA_character_)
})

test_that("parse_engine_line_cpp returns correct value for score tag", {
  engine_line <- 'info depth 1 seldepth 1 time 15 nodes 5 score cp 37'
  expect_equal(parse_engine_line_cpp(engine_line, 'score'), 'cp 37')
})

test_that("parse_engine_line_cpp returns NA for non-existent tag", {
  engine_line <- 'info depth 1 seldepth 1 time 15 nodes 5 score cp 37'
  expect_true(is.na(parse_engine_line_cpp(engine_line, 'nonexistent')))
})

test_that("parse_engine_line_cpp correctly handles string tags", {
  engine_line <- 'info string this is a test string'
  expect_equal(parse_engine_line_cpp(engine_line, 'string'),
               'this is a test string')
})

test_that("parse_engine_line_cpp correctly handles move tags with string", {
  engine_line <- 'info pv d2d4 d7d5 c2c4 e7e6 string test'
  expect_equal(parse_engine_line_cpp(engine_line, 'pv'), 'd2d4 d7d5 c2c4 e7e6')
})

test_that("parse_engine_line_cpp correctly handles currline tags", {
  engine_line <- 'info currline d2d4 d7d5 c2c4 e7e6 string test'
  expect_equal(parse_engine_line_cpp(engine_line, 'currline'),
               'd2d4 d7d5 c2c4 e7e6')
  engine_line <- 'info currline 1 d2d4 d7d5 c2c4 e7e6 string test'
  expect_equal(parse_engine_line_cpp(engine_line, tag = 'currline'),
               '1 d2d4 d7d5 c2c4 e7e6')
})

test_that("parse_engine_line_cpp correctly handles promotions", {
  engine_line <- 'bestmove a7a8q ponder g8f6'
  expect_equal(parse_engine_line_cpp(engine_line, 'bestmove'), 'a7a8q')
})

test_that("remove_na_rows_cols removes rows and columns that are all NAs", {
  # Create a matrix with character data
  my_matrix <- matrix(c("A", "B", "C", "D", "E", "F"), nrow = 2, ncol = 3, byrow = TRUE)

  # Add a row of NAs
  my_matrix <- rbind(my_matrix, c(NA, NA, NA))

  # Add a column of NAs
  my_matrix <- cbind(my_matrix, c(NA, NA, NA))

  # Add some NAs that are not part of a row or column that is all NAs
  my_matrix[1, 2] <- NA
  my_matrix[2, 3] <- NA

  # Use the function to remove rows and columns that are all NAs
  new_matrix <- remove_na_rows_cols(my_matrix)

  # Check that the resulting matrix has the expected dimensions
  expect_equal(dim(new_matrix), c(2, 3))

  # Check that the resulting matrix has the expected values
  expect_equal(new_matrix[1, ], c("A", NA, "C"))
  expect_equal(new_matrix[2, ], c("D", "E", NA))
})

test_that("cram_positionlog returns the correct result", {
  positionlog <- c(
    "info depth 1 seldepth 1 multipv 1 score cp 13 nodes 20 nps 20000 tbhits 0 time 1 pv e2e4",
    "info depth 2 seldepth 2 multipv 1 score cp 14 nodes 40 nps 40000 tbhits 0 time 1 pv e2e4 e7e5",
    "bestmove e2e4 ponder e7e5"
  )

  result <- cram_positionlog(positionlog)

  expect_type(result, "list")
  expect_equal(nrow(result), 2)
  expect_equal(colnames(result), c("depth", "multipv", "score", "pv"))
})

test_that("cram_positionlog handles the all_tags parameter correctly", {
  positionlog <- c(
    "info depth 1 seldepth 1 multipv 1 score cp 13 nodes 20 nps 20000 tbhits 0 time 1 pv e2e4",
    "info depth 2 seldepth 2 multipv 1 score cp 14 nodes 40 nps 40000 tbhits 0 time 1 pv e2e4 e7e5",
    "bestmove e2e4 ponder e7e5"
  )
  result_all_false <- cram_positionlog(positionlog, all_tags = FALSE)
  result_all_true <- cram_positionlog(positionlog, all_tags = TRUE)

  expect_true(ncol(result_all_true) > ncol(result_all_false))
})

positionlog <- c(
  'Stockfish 13 by the Stockfish developers (see AUTHORS file)',
  'readyok',
  'info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled',
  'info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4',
  'bestmove d2d4'
)
test_that('cram_positionlog correctly handles 1-line positions', {
  crammed_positionlog <- cram_positionlog(positionlog)
  expect_equal(nrow(crammed_positionlog), 1)
})
