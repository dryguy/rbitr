ucilog <- list(c(
  "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
  "readyok",
  "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
  "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
  "bestmove d2d4"
))
test_that('parse_ucilog parses ucilogs', {
  parsed_log <- parse_ucilog(ucilog)
  expect_identical(parsed_log, list(list(scores = '146', pvs = 'd2d4')))
  expect_error(parse_ucilog(ucilog, depth = 0))
  expect_error(parse_ucilog(ucilog, depth = 3))
})
