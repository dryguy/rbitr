test_that("cram_positionlog returns the correct result", {
  positionlog <- c(
    "info depth 1 seldepth 1 multipv 1 score cp 13 nodes 20 nps 20000 tbhits 0 time 1 pv e2e4",
    "info depth 2 seldepth 2 multipv 1 score cp 14 nodes 40 nps 40000 tbhits 0 time 1 pv e2e4 e7e5",
    "bestmove e2e4 ponder e7e5"
  )

  result <- cram_positionlog(positionlog)

  expect_type(result, "list")
  expect_equal(nrow(result), length(positionlog))
  expect_equal(colnames(result), c("depth", "multipv", "score", "pv", "bestmove"))
})

test_that("cram_positionlog handles the all parameter correctly", {
  positionlog <- c(
    "info depth 1 seldepth 1 multipv 1 score cp 13 nodes 20 nps 20000 tbhits 0 time 1 pv e2e4",
    "info depth 2 seldepth 2 multipv 1 score cp 14 nodes 40 nps 40000 tbhits 0 time 1 pv e2e4 e7e5",
    "bestmove e2e4 ponder e7e5"
  )

  result_all_false <- cram_positionlog(positionlog, all = FALSE)
  result_all_true <- cram_positionlog(positionlog, all = TRUE)

  expect_true(ncol(result_all_true) > ncol(result_all_false))
})

test_that("cram_positionlog handles custom patterns and column names correctly", {
  positionlog <- c(
    "info depth 1 seldepth 1 multipv 1 score cp 13 nodes 20 nps 20000 tbhits 0 time 1 pv e2e4",
    "info depth 2 seldepth 2 multipv 1 score cp 14 nodes 40 nps 40000 tbhits 0 time 1 pv e2e4 e7e5",
    "bestmove e2e4 ponder e7e5"
  )

  patterns <- c("(?<=\\bnodes\\s)\\s*(\\d+)")
  column_names <- c("nodes")

  result <- cram_positionlog(positionlog, patterns = patterns, column_names = column_names)

  expect_true("nodes" %in% colnames(result))
})

test_that("cram_positionlog correctly extracts data using built-in regex patterns", {
  positionlog <- c(
    "info depth 1 seldepth 1 time 100 nodes 20 pv e2e4 multipv 1 score cp 13 currmove e2e4 currmovenumber 1 hashfull 50 nps 20000 tbhits 0 sbhits 0 cpuload 50 refutation e7e5 currline e2e4 string test",
    "info depth 2 seldepth 2 time 200 nodes 40 pv e2e4 e7e5 multipv 1 score cp 14 currmove e7e5 currmovenumber 2 hashfull 60 nps 40000 tbhits 0 sbhits 0 cpuload 60 refutation e7e6 currline e2e4 e7e5 string test",
    "bestmove e2e4 ponder e7e5"
  )

  result <- cram_positionlog(positionlog, all = TRUE)

  expect_equal(result$depth, c(1, 2, NA))
  expect_equal(result$seldepth, c(1, 2, NA))
  expect_equal(result$time, c(100, 200, NA))
  expect_equal(result$nodes, c(20, 40, NA))
  expect_equal(result$pv, c("e2e4", "e2e4 e7e5", NA))
  expect_equal(result$multipv, c(1, 1, NA))
  expect_equal(result$score, c("cp 13", "cp 14", NA))
  expect_equal(result$currmove, c("e2e4", "e7e5", NA))
  expect_equal(result$currmovenumber, c(1, 2, NA))
  expect_equal(result$hashfull, c(50, 60, NA))
  expect_equal(result$nps, c(20000, 40000, NA))
  expect_equal(result$tbhits, c(0, 0, NA))
  expect_equal(result$sbhits, c(0, 0, NA))
  expect_equal(result$cpuload, c(50, 60, NA))
  expect_equal(result$string, c("test", "test", NA))
  expect_equal(result$refutation, c("e7e5", "e7e6", NA))
  expect_equal(result$currline, c("e2e4", "e2e4 e7e5", NA))
  expect_equal(result$bestmove, c(NA, NA,"e2e4"))
  expect_equal(result$ponder,c(NA ,NA ,"e7e5"))
})
