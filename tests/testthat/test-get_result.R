test_that("get_result correctly identifies game termination markers", {
  # Test a game that ends with "1-0"
  expect_equal(get_result("1.e4 e5 2.Nf3 Nc6 3.Bb5 a6 4.Ba4 Nf6 1-0"), "1-0")

  # Test a game that ends with "0-1"
  expect_equal(get_result("1.e4 c5 2.Nf3 d6 3.d4 cxd4 4.Nxd4 Nf6 0-1"), "0-1")

  # Test a game that ends with "1/2-1/2"
  expect_equal(get_result("1.e4 c5 2.Nf3 d6 3.d4 cxd4 1/2-1/2"), "1/2-1/2")

  # Test a game that ends with "*"
  expect_equal(get_result("1.e4 e5 2.Nf3 Nc6 3.Bb5 a6 4.Ba4 Nf6 *"), "*")

  # Test a game with no termination marker
  expect_equal(get_result("1.e4 e5 2.Nf3 Nc6 3.Bb5 a6 4.Ba4 Nf6"), NA)
})
