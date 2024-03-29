% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_movetext.R
\name{clean_movetext}
\alias{clean_movetext}
\title{Remove comments and annotations from movetext}
\usage{
clean_movetext(movetext, cut_gtm = TRUE)
}
\arguments{
\item{movetext}{A character vector of PGN movetext data}

\item{cut_gtm}{(Default = TRUE) A boolean indicating if game termination
markers should be deleted}
}
\value{
A character vector of clean movetext.
}
\description{
Removes comments, annotations, and other formatting that would interfere with
engine analysis.
}
\details{
The \href{http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.2}{PGN specification}
allows comments, annotations, game termination markers, and formatting
oddities that may not play well with all software. The \code{clean_movetext()}
function removes everything but the moves and move numbers, unless the
\code{cut_gtm} parameter is set to \code{FALSE}.
}
\note{
Game termination markers ('1-0', '0-1', '1/2-1/2', '*') are removed by
default. This causes the movetext to no longer comply with the PGN
specification. PGN files saved without game termination markers will not
function properly. If you intend to save the clean movetext to a PGN file,
set the \code{cut_gtm} parameter to \code{FALSE} to preserve the game termination
markers.

As a human-readable (and writable) format, pgn files may sometimes
contain non-standard characters. Some sources may use zero for castling
('0-0' or '0-0-0') instead of capital letter O. Clean movetext will convert
these to the accepted forms. Em (—) or en (–) dashes are replaced with
ordinary dashes (-).
}
\examples{
clean_movetext('1. e4! {Best by test.} e5 (d5 leads to the Scandinavian.) *')
}
\seealso{
\code{\link[=get_pgn]{get_pgn()}} to get the movetext and other data from a PGN
file.
}
