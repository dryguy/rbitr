% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_gamelog.R
\name{parse_gamelog_score}
\alias{parse_gamelog_score}
\title{Extract the scores from rbitr's \code{evaluate_game()} output}
\usage{
parse_gamelog_score(gamelog, target, depth)
}
\arguments{
\item{gamelog}{A list of engine output from rbitr's \code{evaluate_game()}
function.}

\item{target}{A single-element character vector of the output to return.
Allowed values are 'score' for the evaluation in centipawns, 'pv' for the
principal variation, or 'bestmove' for the best move.}

\item{depth}{(Optional, default = NULL) A single-element integer vector
indicating which search depth to return. The value must not be less than 1
or greater than the maximum depth reported by the engine. A value of NULL
returns data for the maximum depth.}
}
\value{
A list of character vectors of the extracted scores for each
position.
}
\description{
\code{parse_gamelog_score()} is an internal function called by
\code{parse_gamelog()}.
}
\details{
Returns the engine's calculated evaluation (score) for each
position in the game.  See the documentation for \code{parse_gamelog()} for
full details.
}
\seealso{
\itemize{
\item \code{\link[=parse_gamelog]{parse_gamelog()}}
}
}
