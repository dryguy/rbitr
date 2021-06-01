test_that('get_imb gets inaccuracies, mistakes, and blunders', {
  scores <- c(5000, 10, 61, -40, 61, 10, 166)
  moves <- rep('a', 8)
  bestmoves <- rep('b', 9)
  imb <- get_imb(scores, moves, bestmoves, color = 'white')
  expect_equal(imb, list(inaccuracies = 3,
                         mistakes     = 2,
                         blunders     = 1))
  imb <- get_imb(scores, moves, bestmoves, color = 'black')
  expect_equal(imb, list(inaccuracies = 1,
                         mistakes     = 2,
                         blunders     = 3))
  imb <- get_imb(scores, moves, bestmoves, color = 'white',
                 cap_action = "exclude")
  expect_equal(imb, list(inaccuracies = 3,
                         mistakes     = 2,
                         blunders     = integer(0)))
  imb <- get_imb(scores, moves, bestmoves, color = 'white',
                 cap_action = "none")
  expect_equal(imb, list(inaccuracies = 3,
                         mistakes     = 2,
                         blunders     = 1))
  scores <- c(166, 166, 10, 61, -40, 61, 10, 166)
  imb <- get_imb(scores, moves, bestmoves, color = 'white', to_move = 'black')
  expect_equal(imb, list(inaccuracies = 4,
                         mistakes     = 3,
                         blunders     = 2))
  imb <- get_imb(scores, moves, bestmoves, color = 'black', to_move = 'black')
  expect_equal(imb, list(inaccuracies = 2,
                         mistakes     = 3,
                         blunders     = 4))
})
