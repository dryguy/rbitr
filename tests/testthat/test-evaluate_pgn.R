pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'dr_who.pgn'
)
engine <- bigchess::uci_engine('//stockfish_13_win_x64_bmi2.exe')
evaluations <- evaluate_pgn(pgn_path, engine, n_pv = 2, depth = 2)
