test_that("get_coercion correctly calculates coercion", {
  positionlog <- c(
    "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
    "readyok",
    "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
    "info depth 1 seldepth 1 multipv 1 score cp 161 nodes 59 nps 59000 tbhits 0 time 1 pv b1c3",
    "info depth 1 seldepth 1 multipv 2 score cp 146 nodes 59 nps 59000 tbhits 0 time 1 pv d2d4",
    "info depth 2 seldepth 2 multipv 1 score cp 195 nodes 147 nps 73500 tbhits 0 time 2 pv d2d4 g8f6",
    "info depth 2 seldepth 2 multipv 2 score cp 170 nodes 147 nps 73500 tbhits 0 time 2 pv h2h4 g8f6 h4g5",
    "bestmove d2d4 ponder g8f6"
  )
  expect_equal(get_coercion(positionlog), 25)
})
