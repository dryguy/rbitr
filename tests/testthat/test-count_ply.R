test_that("count_ply works correctly", {
  expect_equal(count_ply("1. e4 e5 2. Nf3 Nc6"), 5)
  expect_equal(count_ply("1. e4 e5"), 3)
  expect_equal(count_ply("e2e4 e7e5"), 3)
})

test_that("count_halfmoves works correctly", {
  expect_equal(count_halfmoves("1. e4 e5 2. Nf3 Nc6"), 4)
  expect_equal(count_halfmoves("1. e4 e5"), 2)
  expect_equal(count_halfmoves("e2e4 e7e5"), 2)
})

test_that("count_ply and count_halfmoves validate input", {
  expect_error(count_ply(1))
  expect_error(count_halfmoves(1))
  expect_error(count_ply(c("1. e4 e5 2. Nf3 Nc6", "1. e4 e5 2. Nf3 Nc6")))
  expect_error(count_halfmoves(c("1. e4 e5 2. Nf3 Nc6", "1. e4 e5 2. Nf3 Nc6")))
})
