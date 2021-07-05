pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'test'
)
engine_path <- '//stockfish_14_x64_avx2.exe'

test_that('evaluate_pgn evaluates a pgn', {
  pgn_path <- file.path(pgn_path, 'dr_who.pgn')
  pgnlog <- evaluate_pgn(pgn_path, engine_path, n_pv = 2, limiter = 'depth',
                              limit = 2, mute = TRUE)
  expect_identical(class(pgnlog), 'list')
  expect_identical(pgnlog[[3]][[62]][2], 'readyok')
  expect_identical(pgnlog[[2]][[78]][5], 'bestmove (none)')
})
# save_logs = F & dir.exists(progress_path) = T
test_that('evaluate_pgn warns when save_logs may be forgotten', {
  expect_warning(evaluate_pgn(
    file.path(pgn_path, 'short_game.pgn'),
    engine_path, limiter = 'nodes', limit = 100, mute = TRUE, save_logs = FALSE
  ))
})
# save_logs = T & dir.exists(progress_path) = F
test_that('evaluate_pgn creates save logs', {
  if (dir.exists(file.path(pgn_path, 'shortest_game'))) {
    stop(paste0(
      'The directory shortest_game already exists.\n',
      '* The test cannot run unless the directory is deleted.'
    ))
  }
  evaluate_pgn(
    file.path(pgn_path, 'shortest_game.pgn'),
    engine_path, limiter = 'nodes', limit = 100, mute = TRUE, save_logs = TRUE,
    save_path = file.path(pgn_path, 'shortest_game')
  )
  expect_identical(TRUE, dir.exists(file.path(pgn_path, 'shortest_game')))
  expect_identical(
    TRUE,
    file.exists(
      file.path(pgn_path, 'shortest_game/shortest_game_nodes100pv1_1.RData')
    )
  )
  unlink(file.path(pgn_path, 'shortest_game'), recursive = TRUE)
})
# mute = FALSE
test_that('evaluate_pgn reports progress with mute = FALSE', {
  expect_output(evaluate_pgn(
    file.path(pgn_path, 'shortest_game.pgn'),
    engine_path, limiter = 'nodes', limit = 100, mute = FALSE
  ))
})

