test_that("autoplay_game works", {
  test <- autoplay_game('/stockfish.exe', limiter = 'depth', limit = 1,
                        n_cpus = 1, mute = TRUE, ply_limit = 3)
  expect_true(is.character(test))
  expect_equal(length(test), 1)
})

test_that("append_result works", {
  expect_equal(append_result('1. f3 e6 2. g4 Qh4#'),
               '1. f3 e6 2. g4 Qh4# 0-1')
})
