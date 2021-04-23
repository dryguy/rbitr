gamelog <- list(c(
  "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
  "readyok",
  "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
  "info depth 1 seldepth 1 multipv 1 score cp 146 nodes 30 nps 30000 tbhits 0 time 1 pv d2d4",
  "bestmove d2d4"
))
test_that('parse_gamelog parses gamelogs', {
  expect_identical(parse_gamelog(gamelog, 'score'), '146')
  expect_identical(parse_gamelog(gamelog, 'pv'), 'd2d4')
  expect_identical(parse_gamelog(gamelog, 'bestmove'), 'd2d4')
  expect_error(parse_gamelog(gamelog, 'score', depth = 0))
  expect_error(parse_gamelog(gamelog, 'score', depth = 3))
})
