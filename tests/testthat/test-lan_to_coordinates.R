test_that("lan_to_coordinates correctly converts LAN to coordinates", {
  # Test a move without promotion
  result <- lan_to_coordinates("e2e4")
  expect_equal(result[[1]], c(5, 2))  # Original coordinates
  expect_equal(result[[2]], c(5, 4))  # Destination coordinates
  expect_null(result[[3]])            # Promoted piece

  # Test a move with promotion
  result <- lan_to_coordinates("e7e8q")
  expect_equal(result[[1]], c(5, 7))  # Original coordinates
  expect_equal(result[[2]], c(5, 8))  # Destination coordinates
  expect_equal(result[[3]], "q")      # Promoted piece
})

test_that("lan_to_coordinates handles invalid inputs", {
  # Test a move with invalid characters
  expect_error(lan_to_coordinates("z9z0"))

  # Test a move with too few characters
  expect_error(lan_to_coordinates("e2"))

  # Test a move with too many characters
  expect_error(lan_to_coordinates("e2e4qk"))
})
