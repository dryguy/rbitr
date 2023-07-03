test_that('regan_antiderivative returns the antideriv. of 1 / (1 + abs(x))', {
  expect_equal(regan_antiderivative(-9), -log(10))
  expect_identical(regan_antiderivative(NA_real_), NA_real_)
  expect_identical(regan_antiderivative(NaN), NaN)
  expect_identical(regan_antiderivative(Inf), NaN)
  x <- c(-10, -1, 0, 1, 10)
  y <- c(-log(11), -log(4) / 2, 0, log(4) / 2, log(11))
  expect_equal(regan_antiderivative(x), y)
})
