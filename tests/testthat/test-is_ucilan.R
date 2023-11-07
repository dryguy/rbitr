test_that("is_ucilan correctly identifies valid and invalid UCI LAN strings", {
  # Test valid UCI LAN string
  expect_true(is_ucilan("e2e4 e7e5 g1f3 b8c6 e7e8q"))

  # Test invalid UCI LAN strings
  expect_false(is_ucilan("e2 e7e5 g1f3 b8c6"))
  expect_false(is_ucilan("1. e4 e5 2. Nc3"))

  # Test non-string inputs
  expect_error(is_ucilan(123))
  expect_error(is_ucilan(TRUE))
})
