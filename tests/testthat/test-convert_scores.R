scores <- c("90", "-26", "mate 2" , "mate -1", "mate 1", NA)
test_that('convert_scores converts scores', {
  expect_equal(convert_scores(scores), c(90, 26, 5000, 5000, 5000, NA))
})
