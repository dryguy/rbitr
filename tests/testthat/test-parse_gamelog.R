gamelog <- list(
  c(
    "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
    "readyok",
    "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
    "info depth 1 seldepth 1 multipv 1 score cp 161 nodes 59 nps 59000 tbhits 0 time 1 pv b1c3",
    "info depth 1 seldepth 1 multipv 2 score cp 146 nodes 59 nps 59000 tbhits 0 time 1 pv d2d4",
    "info depth 2 seldepth 2 multipv 1 score cp 195 nodes 147 nps 73500 tbhits 0 time 2 pv d2d4 g8f6",
    "info depth 2 seldepth 2 multipv 2 score cp 170 nodes 147 nps 73500 tbhits 0 time 2 pv h2h4 g8f6 h4g5",
    "bestmove d2d4 ponder g8f6"
  ),
  c(
    "Stockfish 13 by the Stockfish developers (see AUTHORS file)",
    "readyok",
    "info string NNUE evaluation using nn-62ef826d1a6d.nnue enabled",
    "info depth 1 seldepth 1 multipv 1 score cp -121 nodes 42 nps 42000 tbhits 0 time 1 pv b8c6",
    "info depth 1 seldepth 1 multipv 2 score cp -136 nodes 42 nps 42000 tbhits 0 time 1 pv f8g7",
    "info depth 2 seldepth 2 multipv 1 score cp -154 nodes 119 nps 119000 tbhits 0 time 1 pv f8g7 d2d4",
    "info depth 2 seldepth 2 multipv 2 score cp -154 nodes 119 nps 119000 tbhits 0 time 1 pv d7d6 d2d4",
    "bestmove f8g7 ponder d2d4"
  )
)
test_that('parse_gamelog parses gamelogs', {
  expect_identical(parse_gamelog(gamelog, 'score')[[1]], c('195', '170'))
  expect_identical(parse_gamelog(gamelog, 'pv')[[2]], c('f8g7 d2d4', 'd7d6 d2d4'))
  expect_identical(parse_gamelog(gamelog, 'bestmove'), c('d2d4', 'f8g7'))
  expect_error(parse_gamelog(gamelog, 'score', depth = 0))
  expect_error(parse_gamelog(gamelog, 'score', depth = 3))
})
test_that("parse_gamelog_score returns scores", {
  score_test <- parse_gamelog_score(gamelog, target = 'score', depth = 2)
  expect_identical(score_test, list(c('195', '170'), c('-154', '-154')))
})
test_that("parse_gamelog_score returns PVs", {
  score_test <- parse_gamelog_score(gamelog, target = 'pv', depth = 2)
  expect_identical(score_test, list(c('d2d4 g8f6', 'h2h4 g8f6 h4g5'),
                                    c('f8g7 d2d4', 'd7d6 d2d4')))
})
test_that("parse_gamelog_bestmove returns best moves", {
  test <- parse_gamelog_bestmove(gamelog)
  expect_identical(test, c('d2d4', 'f8g7'))
})
test_that("parse_gamelog handles gamelogs with extra or missing pvs", {
  gamelog_path <- file.path(
    system.file(package = 'rbitr'),
    'extdata',
    'test',
    'parse_gamelog_test_data.R'
  )
  load(gamelog_path)
  test_parse <- parse_gamelog(gamelog, target = 'score')
  expect_identical(test_parse, list(c("-19", "-22", "-25", "-42", "-48", "-59",
                                 "-59", "-68", "-69", "-84", "-87", "-95",
                                 "-95", "-113", "-132", "-140", "-155", "-202",
                                 "-205", "-209")))
})

# Load the locations of the chess engines that will be tested
engines_file_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'engine_paths.R'
)
source(engines_file_path)
movetext <- '1. f3 e6 2. g4'

for (engine_path in engine_paths) {
  test_that("parse_gamelog_score works with various engines", {
    gamelog <- evaluate_game(movetext, engine_path, limiter = 'depth',
                             limit = 3, n_pv = 3)
    test <- parse_gamelog_score(gamelog, target = 'score', depth = 3)
    expect_identical(class(test), 'list')
    expect_identical(class(test[[1]]), 'character')
    expect_identical(length(test), 4L)
  })
}
for (engine_path in engine_paths) {
  test_that("parse_gamelog_bestmove works with various engines", {
    gamelog <- evaluate_game(movetext, engine_path, limiter = 'depth',
                             limit = 3, n_pv = 3)
    test <- parse_gamelog_bestmove(gamelog)
    expect_identical(class(test), 'character')
    expect_identical(class(test[[1]]), 'character')
    expect_identical(length(test), 4L)
    for (i in 1:length(test)) {
      expect_identical(stringr::str_match(test[[i]],
                                          '[a-h][1-8][a-h][1-8]')[1, ],
                       test[[i]])
    }
  })
}
