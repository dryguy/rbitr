engine <- bigchess::uci_engine('//stockfish_13_win_x64_bmi2.exe')
pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'dr_who.pgn'
)
pgn <- read_pgn(pgn_path)
movetext <- pgn$Movetext[[1]]
