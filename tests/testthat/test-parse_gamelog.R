gamelog <- list(
  c(
    "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
    "readyok",
    "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
    "info depth 1 seldepth 1 multipv 1 score cp 161 nodes 59 nps 59000 tbhits 0 time 1 pv b1c3",
    "info depth 1 seldepth 1 multipv 2 score cp 146 nodes 59 nps 59000 tbhits 0 time 1 pv d2d4",
    "info depth 2 seldepth 2 multipv 1 score cp 195 nodes 147 nps 73500 tbhits 0 time 2 pv d2d4 g8f6",
    "info depth 2 seldepth 2 multipv 2 score cp 170 nodes 147 nps 73500 tbhits 0 time 2 pv h2h4 g8f6 h4g5",
    "bestmove d2d4 ponder g8f6"
  ),
  c(
    "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
    "readyok",
    "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
    "info depth 1 seldepth 1 multipv 1 score cp -121 nodes 42 nps 42000 tbhits 0 time 1 pv b8c6",
    "info depth 1 seldepth 1 multipv 2 score cp -136 nodes 42 nps 42000 tbhits 0 time 1 pv f8g7",
    "info depth 2 seldepth 2 multipv 1 score cp -154 nodes 119 nps 119000 tbhits 0 time 1 pv f8g7 d2d4",
    "info depth 2 seldepth 2 multipv 2 score cp -154 nodes 119 nps 119000 tbhits 0 time 1 pv d7d6 d2d4",
    "bestmove f8g7 ponder d2d4"
  )
)
test_that('parse_gamelog parses gamelogs', {
  expect_identical(parse_gamelog(gamelog, 'score')[[1]], c('195', '170'))
  expect_identical(parse_gamelog(gamelog, 'pv')[[2]], c('f8g7 d2d4', 'd7d6 d2d4'))
  expect_identical(parse_gamelog(gamelog, 'bestmove'), list('d2d4', 'f8g7'))
  expect_error(parse_gamelog(gamelog, 'score', depth = 0))
  expect_error(parse_gamelog(gamelog, 'score', depth = 3))
})
