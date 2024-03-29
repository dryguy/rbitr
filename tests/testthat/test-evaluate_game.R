# Load the locations of the chess engines that will be tested
engines_file_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'engine_paths.R'
)
source(engines_file_path)

test_that('evaluate_game evaluates a game', {
  movetext <- '1. e4 g5 2. Nc3 f5 3. Qh5# 1-0'
  for (engine_path in engine_paths) {
    analysis <- evaluate_game(movetext, engine_path,
                              limiter = 'depth', limit = 1)
    expect_identical(class(analysis), 'list')
    expect_identical(analysis[[1]][2], 'readyok')
    expect_identical(analysis[[6]][5], 'bestmove (none)')
    analysis <- evaluate_game(movetext, engine_path,
                              limiter = 'nodes', limit = 10)
    expect_identical(analysis[[1]][2], 'readyok')
    analysis <- evaluate_game(movetext, engine_path,
                              limiter = 'movetime', limit = 100)
    expect_identical(analysis[[1]][2], 'readyok')
    expect_output(evaluate_game('1. e4 e5', engine_path, limiter = 'nodes',
                                limit = 10, mute = FALSE))
    analysis <- evaluate_game(movetext, engine_path,
                              limiter = 'depth', limit = 1, hash_size = 32)
    expect_identical(analysis[[6]][4], 'info depth 0 score mate 0')
  }
})
