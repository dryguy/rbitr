% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_movetext.R
\name{parse_movetext}
\alias{parse_movetext}
\title{Parse movetext}
\usage{
parse_movetext(
  movetext,
  cmd_name,
  first_eval = NULL,
  mate0 = FALSE,
  mate_value = 50000
)
}
\arguments{
\item{movetext}{A character vector of movetext, where each vector entry
is for a separate game.}

\item{cmd_name}{A single-element character vector of the command to parse}

\item{first_eval}{(Default = NULL) A single-element integer vector indicating
what value should be assigned to the initial position, before white's first
move. The default (NULL) omits the first evaluation.}

\item{mate0}{(Default = FALSE) A single-element boolean vector indicating
whether to include a value for mated positions ('mate in 0').}

\item{mate_value}{(Default = 35000) The value (in centipawns) to assign to
positions evaluated as 'mate in x'.}
}
\value{
A list containing numeric vectors of the specified tag value. Each
list entry is for a separate game. For \code{get_clocks()}, the value will be
converted to seconds.
}
\description{
The function \code{parse_movetext()} is the parser behind \code{get_clocks()} and
\code{get_evals()}.
}
