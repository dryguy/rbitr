test_that('clean_movetext removes semicolon format comments', {
  expect_identical(clean_movetext('1. e4 ; Best by test.\n e5'), '1. e4 e5')
})
test_that('clean_movetext removes line breaks', {
  expect_identical(clean_movetext('1. e4 e5\n2. d4'), '1. e4 e5 2. d4')
})
test_that('clean_movetext removes brace format comments', {
  expect_identical(clean_movetext('1. e4 {Best by test.}'), '1. e4')
})
test_that('clean_movetext removes 1... style numbering', {
  expect_identical(clean_movetext('1. e4 1... e5'), '1. e4 e5')
})
test_that('clean_movetext removes traditional annotations', {
  expect_identical(clean_movetext('1. e4!! e5! 2. d4!?'), '1. e4 e5 2. d4')
})
test_that('clean_movetext removes numerical annotation glyphs (NAGs)', {
  expect_identical(clean_movetext('1. e4 $32 e5'), '1. e4 e5')
})
test_that('clean_movetext removes extra spaces', {
  expect_identical(clean_movetext('1. e4    e5'), '1. e4 e5')
})
test_that('clean_movetext removes results', {
  expect_identical(clean_movetext('1. e4 1-0'), '1. e4')
  expect_identical(clean_movetext('1. e4 0-1'), '1. e4')
  expect_identical(clean_movetext('1. e4 1/2-1/2'), '1. e4')
  expect_identical(clean_movetext('1. e4 *'), '1. e4')
})
