scores <- c(12, 171, -72, 50000, 50000)
moves <- c("g2g4", "e7e6", "f2f4", "d8h4")
bestmoves <- c("d2d4", "d7d5", "g1f3", "d8h4")
imb <- get_imb(scores, moves, bestmoves, color = 'white')
test_that('get_imb gets inaccuracies, mistakes, and blunders', {
  expect_equal(imb, list(inaccuracies = 0, mistakes = 0, blunders = 0))
})
