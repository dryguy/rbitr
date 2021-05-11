% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_pgn.R
\name{evaluate_pgn}
\alias{evaluate_pgn}
\title{Evaluate each chess game in a pgn file.}
\usage{
evaluate_pgn(
  pgn_path,
  engine,
  n_pv,
  go_mode,
  go_value,
  mute = FALSE,
  save_logs = FALSE
)
}
\arguments{
\item{pgn_path}{A single-element character vector of the path to the pgn.}

\item{engine}{An engine handler generated by the
\href{https://github.com/rosawojciech/bigchess}{bigchess} function
\code{uci_engine()}.}

\item{n_pv}{A single-element integer vector of the desired number of
principal variations.}

\item{go_mode}{A single-element character vector indicating the desired
mode of search termination. Allowed values are 'depth' (to search a fixed
number of plies), 'nodes' (to search a fixed number of nodes), and
'movetime' (to search a fixed number of milliseconds).}

\item{go_value}{A single-element integer vector of the desired search depth
(# of plies), search nodes (# of nodes), or search time (# of mseconds).}

\item{mute}{(Boolean, default = FALSE) Suppress progress report?}

\item{save_logs}{(Boolean, default = FALSE) Save progress? Recommended for
long analyses in case it has to be stopped before finishing all games.}
}
\value{
A list of gamelogs (see \code{evaluate_game()} for details).
}
\description{
Evaluate each chess game in a pgn file.
}
\details{
A wrapper for rbitr's \code{evaluate_game()} function that evaluates each
game in the specified pgn file. Note that this can take a very long time
for pgn files with a lot of games, and even longer when searching high
numbers of principal variations and/or deep evaluations. For long runs, it
is recommended to set save_logs = TRUE. This will save the evaluation of
each game to a folder with the same name as the pgn. If the analysis has to
be stopped and restarted, it will skip games that have already been
analyzed and pick up where it left off.
}
\examples{
library(bigchess)
# Modify engine_path as required for your engine location & operating system
engine_path <- '//stockfish_13_win_x64_bmi2.exe'
engine <- uci_engine(engine_path)
pgn_path <- file.path(
  system.file(package = 'rbitr'),
  'extdata',
  'fools_mate.pgn'
)
evaluate_pgn(pgn_path, engine, n_pv = 1, go_mode = 'depth', go_value = 1)
}