test_that("get_engine_match() correctly counts engine matches", {
  movetext <- '1. e4 e5 2. d4 d5'
  gamelog <- evaluate_game(movetext, '//stockfish.exe', limiter = 'depth',
                           limit = 1)
  match_ratios <- get_engine_match(gamelog, movetext)
  expect_identical(is.list(match_ratios), TRUE)
  expect_identical(length(match_ratios), 5L)
})

test_that("get_engine_match() correctly handles single move", {
  movetext <- '1. e4'
  gamelog <- evaluate_game(movetext, '//stockfish.exe', limiter = 'depth',
                           limit = 1)
  match_ratios <- get_engine_match(gamelog, movetext)
  expect_identical(is.list(match_ratios), TRUE)
  expect_identical(length(match_ratios), 5L)
})


test_that("get_engine_match() correctly handles empty movetext", {
  movetext <- ''
  gamelog <- evaluate_game(movetext, '//stockfish.exe', limiter = 'depth',
                           limit = 1)
  expect_identical(get_engine_match(gamelog, movetext)$matches, NA)
  expect_identical(get_engine_match(gamelog, movetext)$black_ratio, NA)
  expect_identical(get_engine_match(gamelog, movetext)$white_ratio, NA)
  expect_identical(get_engine_match(gamelog, movetext)$black_matches, NA)
  expect_identical(get_engine_match(gamelog, movetext)$white_matches, NA)
})
