test_that("is_uci_move works as expected", {
  expect_true(is_uci_move("e2e4"))
  expect_true(is_uci_move("e7e8q"))
  expect_false(is_uci_move("a2a9"))
  expect_false(is_uci_move("e2e"))
  expect_false(is_uci_move("e2e4x"))
  expect_false(is_uci_move("20000"))
})

test_that("parse_split_engine_line returns correct value for numeric tags", {
  engine_line <-
    "info depth 2 seldepth 2 multipv 2 score cp -154 nodes 119 nps 119000 tbhits 0 time 1 pv d7d6 d2d4"
  split_engine_line <- strsplit(engine_line, split = ' ', fixed = TRUE)[[1]]
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'depth'), '2')
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'seldepth'), '2')
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'multipv'), '2')
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'nodes'), '119')
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'nps'), '119000')
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'tbhits'), '0')
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'time'), '1')
})

test_that("parse_split_engine_line returns correct value for move tags", {
  engine_line <- 'bestmove d2d4 ponder g8f6'
  split_engine_line <- strsplit(engine_line, split = ' ', fixed = TRUE)[[1]]
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'bestmove'),
               'd2d4')
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'ponder'),
               'g8f6')
  engine_line <-
    "info depth 2 seldepth 2 multipv 2 score cp -154 nodes 119 nps 119000 tbhits 0 time 1 pv d7d6 d2d4"
  split_engine_line <- strsplit(engine_line, split = ' ', fixed = TRUE)[[1]]
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'pv'),
               'd7d6 d2d4')
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'currmove'), NA)
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'refutation'),
               NA)
})

test_that("parse_split_engine_line returns correct value for score tag", {
  engine_line <- 'info depth 1 seldepth 1 time 15 nodes 5 score cp 37'
  split_engine_line <- strsplit(engine_line, split = ' ', fixed = TRUE)[[1]]
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'score'),
               'cp 37')
})

test_that("parse_split_engine_line returns NA for non-existent tag", {
  engine_line <- 'info depth 1 seldepth 1 time 15 nodes 5 score cp 37'
  split_engine_line <- strsplit(engine_line, split = ' ', fixed = TRUE)[[1]]
  expect_true(is.na(parse_split_engine_line(split_engine_line,
                                            tag = 'nonexistent')))
})

test_that("parse_split_engine_line correctly handles string tags", {
  engine_line <- 'info string this is a test string'
  split_engine_line <- strsplit(engine_line, split = ' ', fixed = TRUE)[[1]]
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'string'),
               'this is a test string')
})

test_that("parse_split_engine_line correctly handles move tags with string", {
  engine_line <- 'info pv d2d4 d7d5 c2c4 e7e6 string test'
  split_engine_line <- strsplit(engine_line, split = ' ', fixed = TRUE)[[1]]
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'pv'),
               'd2d4 d7d5 c2c4 e7e6')
})

test_that("parse_split_engine_line correctly handles currline tags", {
  engine_line <- 'info currline d2d4 d7d5 c2c4 e7e6 string test'
  split_engine_line <- strsplit(engine_line, split = ' ', fixed = TRUE)[[1]]
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'currline'),
               'd2d4 d7d5 c2c4 e7e6')
  engine_line <- 'info currline 1 d2d4 d7d5 c2c4 e7e6 string test'
  split_engine_line <- strsplit(engine_line, split = ' ', fixed = TRUE)[[1]]
  expect_equal(parse_split_engine_line(split_engine_line, tag = 'currline'),
               '1 d2d4 d7d5 c2c4 e7e6')
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
