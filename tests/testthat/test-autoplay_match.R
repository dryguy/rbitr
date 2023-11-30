test_that("autoplay_match works", {
  test <- autoplay_match('/stockfish.exe', limiter = 'depth', limit = 1,
                        n_cpus = 1, mute = TRUE, ply_limit = 3, n_games = 2)
  expect_true(is.data.frame(test))
  expect_equal(nrow(test), 2)
})
