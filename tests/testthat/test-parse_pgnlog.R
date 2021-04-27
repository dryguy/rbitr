pgnlog <- list(list(c(
  "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
  "readyok",
  "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
  "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
  "bestmove d2d4"
)))
test_that('parse_pgnlog parses pgnlogs', {
  expect_identical(parse_pgnlog(pgnlog, 'score'), list(list('146')))
  expect_identical(parse_pgnlog(pgnlog, 'pv'), list(list('d2d4')))
  expect_identical(parse_pgnlog(pgnlog, 'bestmove'), list(list('d2d4')))
  expect_error(parse_pgnlog(pgnlog, 'score', depth = 0))
  expect_error(parse_pgnlog(pgnlog, 'score', depth = 3))
})
