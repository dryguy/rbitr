test_that("algebraic_to_coordinates returns correct values", {
  # Test 1: Square "a1"
  expect_equal(algebraic_to_coordinates("a1"), c(1, 1))

  # Test 2: Square "h8"
  expect_equal(algebraic_to_coordinates("h8"), c(8, 8))

  # Test 3: Square "e5"
  expect_equal(algebraic_to_coordinates("e5"), c(5, 5))

  # Test 4: Square "d4"
  expect_equal(algebraic_to_coordinates("d4"), c(4, 4))
})
