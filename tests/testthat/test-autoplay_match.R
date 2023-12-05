test_that("autoplay_match works", {
  test <- autoplay_match('/stockfish.exe', limiter = 'depth', limit = 1,
                        n_cpus = 1L, mute = TRUE, ply_limit = 3, n_games = 2)
  expect_true(is.data.frame(test))
  expect_equal(nrow(test), 2)
  pgn_path <- tempfile()
  test1 <- autoplay_match('/stockfish.exe', limiter = 'depth', limit = 1,
                         n_cpus = 1L, mute = TRUE, ply_limit = 3, n_games = 1,
                         save_path = pgn_path)
  test2 <- autoplay_match('/stockfish.exe', limiter = 'depth', limit = 1,
                         n_cpus = 1L, mute = TRUE, ply_limit = 3, n_games = 2,
                         save_path = pgn_path)
  expect_equal(nrow(test1), 1)
  expect_equal(nrow(test2), 2)
  expect_equal(test1[1, ], test2[1, ])
  file.remove(pgn_path)
})
