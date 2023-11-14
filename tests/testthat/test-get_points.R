test_that("get_points correctly converts chess results to points", {
  expect_equal(get_points(c("0-1", "1-0", "1/2-1/2", "*")), c(0, 1, 0.5, NA))
})

test_that("get_points throws an error for invalid results", {
  expect_error(get_points(c("invalid")), "Invalid argument to get_points: invalid. Expected one of '0-1', '1-0', '1/2-1/2', '*'")

})
