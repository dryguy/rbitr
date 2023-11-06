test_that("get_position returns correct positions", {
  expect_equal(get_position("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6", 2), "e2e4 e7e5")
  expect_equal(get_position("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6", 6), "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6")
  expect_equal(get_position("", 0), "")
})

test_that("get_position handles invalid input", {
  expect_error(get_position(123, 2))
  expect_error(get_position("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6", "two"))
})

test_that("get_position handles edge cases", {
  expect_equal(get_position("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6", 1), "e2e4")
  expect_equal(get_position("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6", 0), "")
  expect_error(get_position("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6", 7))
})
