pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'dr_who.pgn'
)
test_that('evaluate_pgn evaluates a pgn', {
  engine <- bigchess::uci_engine('//stockfish_13_win_x64_bmi2.exe')
  evaluations <- evaluate_pgn(pgn_path, engine, n_pv = 2, go_mode = 'depth',
                              go_value = 2, mute = TRUE)
  expect_identical(class(evaluations), 'list')
  expect_identical(evaluations[[3]][[62]][2], 'readyok')
  expect_identical(evaluations[[2]][[78]][5], 'bestmove (none)')
  bigchess::uci_quit(engine)
})
