scores <- c(12, -171, -72, -50000, -50000)
moves <- c("g2g4", "e7e6", "f2f4", "d8h4")
bestmoves <- c("d2d4", "d7d5", "g1f3", "d8h4")
test_that('get_imb gets inaccuracies, mistakes, and blunders', {
  imb <- get_imb(scores, moves, bestmoves, color = 'white')
  expect_equal(imb, list(inaccuracies = integer(0),
                         mistakes     = integer(0),
                         blunders     = c(1, 2)))
})
test_that('get_imb works with cap_action = "exclude"', {
  imb <- get_imb(scores, moves, bestmoves, color = 'white',
                 cap_action = "exclude")
  expect_equal(imb, list(inaccuracies = integer(0),
                         mistakes     = integer(0),
                         blunders     = 1))
})
test_that('get_imb works with cap_action = "none"', {
  imb <- get_imb(scores, moves, bestmoves, color = 'white',
                 cap_action = "none")
  expect_equal(imb, list(inaccuracies = integer(0),
                         mistakes     = integer(0),
                         blunders     = c(1, 2)))
})
test_that('get_imb works with to_move = "black"', {
  scores <- c(-171, -72, -50000, -50000)
  moves <- c("e7e6", "f2f4", "d8h4")
  bestmoves <- c("d7d5", "g1f3", "d8h4")
  imb <- get_imb(scores, moves, bestmoves, color = 'black')
  expect_equal(imb, list(inaccuracies = integer(0),
                         mistakes     = integer(0),
                         blunders     = integer(0)))
})

color <- 'white'
cap <- 1000
cap_action <- 'replace'
first_ply <- 1
to_move <- 'white'
mate <- 50000
diff(c(12, -171, -72, -50000, -50000))
moves == bestmoves
