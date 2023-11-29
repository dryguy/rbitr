test_that("get_cpu_name function", {
  test <- get_cpu_name()

  # Test that the function returns a character string
  expect_type(test, "character")

  # Test that the function does not return NA
  expect_false(is.na(test))

  # Test that the function does not return NULL
  expect_false(is.null(test))

  # Test that the function does not return an empty string
  expect_false(test == "")
})
