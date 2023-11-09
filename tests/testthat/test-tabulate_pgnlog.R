pgnlog <- list(list(c(
  "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
  "readyok",
  "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
  "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
  "bestmove d2d4"
)))
test_that("tabulate_pgnlog condenses a pgnlog", {
  pgnlog_table <- tabulate_pgnlog(pgnlog)
  expect_true(is.list(pgnlog_table))
  expect_equal(dim(pgnlog_table[[1]]), c(1, 5))
  expect_equal(pgnlog_table[[1]]$pv, 'd2d4')
})
