test_that('convert_scores converts scores', {
  scores <- c(
    '90', '-26', '26 upperbound', 'mate -2', 'mate 1', 'mate -1', 'mate 0', NA,
    '26 lowerbound', 'cp 10', 'cp 20'
  )
  expect_equal(
    convert_scores(scores),
    c(90, 26, 26, 5000, 5000, 5000, -5000, NA, 26, -10, 20)
  )
  expect_equal(
    convert_scores(
      c('20', '-20', 'mate 2', 'mate -2'), flip_signs = FALSE),
    c(20, -20, 5000, -5000)
  )
})
