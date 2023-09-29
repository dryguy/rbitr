test_that("get_positions correctly constructs positions", {
  movetext <- "1. e4 e5 2. Nf3 Nc6 3. Bb5"
  positions <- get_positions(movetext)
  expect_equal(positions,
               c("", "1. e4", "1. e4 e5", "1. e4 e5 2. Nf3",
                 "1. e4 e5 2. Nf3 Nc6", "1. e4 e5 2. Nf3 Nc6 3. Bb5"))
})

test_that("get_positions handles empty movetext", {
  movetext <- ""
  positions <- get_positions(movetext)
  expect_equal(positions, "")
})

test_that("get_positions handles single move", {
  movetext <- "1. e4"
  positions <- get_positions(movetext)
  expect_equal(positions, c("", "1. e4"))
})
