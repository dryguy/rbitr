% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplay_game.R
\name{append_result}
\alias{append_result}
\title{Append outcome of chess game to moves in standard algebraic notation (SAN)}
\usage{
append_result(movetext)
}
\arguments{
\item{movetext}{A string of chess moves in standard algebraic notation (SAN).}
}
\value{
A string of chess moves with the game result appended.
}
\description{
The function \code{append_result()} is  helper function for autoplay_game() that
takes a string of chess moves in standard algebraic notation (SAN) as the
argument. If the last character is # it checks to see which side won, and
appends the appropriate result ("1-0" or "0-1").  If the last character is
not #, it assumes stalemate and adds "1/2-1/2 {stalemate}".
}
\examples{
#' append_result('1. f3 e6 2. g4 Qh4#')
}