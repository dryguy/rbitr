test_that("get_engine_match() correctly counts engine matches", {
  movetext <- '1. e4 e5 2. d4 d5'
  gamelog <- evaluate_game(movetext, '//stockfish.exe', limiter = 'depth',
                           limit = 1)
  match_ratios <- get_engine_match(gamelog, movetext)
  expect_identical(list(white_ratio = 0.5, black_ratio = 0), match_ratios)
})

test_that("get_engine_match() correctly handles single move", {
  movetext <- '1. e4'
  gamelog <- evaluate_game(movetext, '//stockfish.exe', limiter = 'depth',
                           limit = 1)
  match_ratios <- get_engine_match(gamelog, movetext)
  expect_identical(list(white_ratio = 1, black_ratio = NA_real_), match_ratios)
})
