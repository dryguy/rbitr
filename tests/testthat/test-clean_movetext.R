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
test_that('clean_movetext removes recursive variations', {
  movetext <- '1. e4 (Fools mate is also possible with colors reversed: 1. g4 (or f3 or f4) e5 (or e6) 2. f3 (or f4 or g4) Qh4#) g5 2. Nc3 f5 3. Qh5# 1-0'
  expect_identical(clean_movetext(movetext), '1. e4 g5 2. Nc3 f5 3. Qh5#')
  })
test_that('clean_movetext removes extra spaces', {
  expect_identical(clean_movetext('1. e4    e5'), '1. e4 e5')
})
test_that('clean_movetext removes game termination markers', {
  expect_identical(clean_movetext('1. e4 1-0'), '1. e4')
  expect_identical(clean_movetext('1. e4 0-1'), '1. e4')
  expect_identical(clean_movetext('1. e4 1/2-1/2'), '1. e4')
  expect_identical(clean_movetext('1. e4 *'), '1. e4')
})
test_that('clean_movetext trims whitespace' ,{
  expect_identical(clean_movetext(' 1. e4 e5 '), '1. e4 e5')
})
