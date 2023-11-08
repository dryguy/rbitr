test_that("count_moves works correctly", {
  expect_equal(count_moves("1. e4 e5 2. Nf3 Nc6"), 2)
  expect_equal(count_moves("1. e4 e5 2. Nf3"), 1.5)
  expect_equal(count_moves("e2e4 e7e5"), 1)
})

test_that("count_ply works correctly", {
  expect_equal(count_ply("1. e4 e5 2. Nf3 Nc6"), 4)
  expect_equal(count_ply("1. e4 e5"), 2)
  expect_equal(count_ply("e2e4 e7e5"), 2)
  expect_equal(count_halfmoves("e2e4 e7e5"), 2)
})

test_that("count_positions works correctly", {
  expect_equal(count_positions("1. e4 e5 2. Nf3 Nc6"), 5)
  expect_equal(count_positions("1. e4 e5"), 3)
  expect_equal(count_positions("e2e4 e7e5"), 3)
  expect_equal(count_nodes("e2e4 e7e5"), 3)
})

test_that("count_ply and count_halfmoves validate input", {
  expect_error(count_ply(1))
  expect_error(count_halfmoves(1))
  expect_error(count_moves(1))
  expect_error(count_ply(c("1. e4 e5", "1. d4 d5")))
  expect_error(count_halfmoves(c("1. e4 e5", "1. d4 d5")))
  expect_error(count_moves(c("1. e4 e5", "1. d4 d5")))
})
