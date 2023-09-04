test_that("count_ply works correctly", {
  expect_equal(count_ply("1. e4 e5 2. Nf3 Nc6"), 4)
  expect_equal(count_ply("1. e4 e5"), 2)
})
