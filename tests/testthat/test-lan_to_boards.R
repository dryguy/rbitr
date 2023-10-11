test_that("lan_to_boards works", {
  lan <- 'e2e4 c7c5'
  boards <- lan_to_boards(lan)
  expect_equal(board_to_fen(boards[[3]]),
               "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2")
})
