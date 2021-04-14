evaluations <- list(list(list(scores = '146', pvs = 'd2d4')))
scores <- extract_eval(evaluations, 'scores')
pvs <- extract_eval(evaluations, 'pvs')
test_that('extract_eval extracts evaluations', {
  expect_identical(scores, list('146'))
  expect_identical(pvs, list('d2d4'))
})
