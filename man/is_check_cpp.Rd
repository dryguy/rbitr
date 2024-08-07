% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{is_check_cpp}
\alias{is_check_cpp}
\title{Test if a king is in check}
\usage{
is_check_cpp(position, p)
}
\arguments{
\item{position}{An 8 x 8 matrix representing the current position.}

\item{p}{An integer indicating the color of the enemy pieces (1 for white,
-1 for black).}
}
\value{
A Boolean indicating if the king is in check.
}
\description{
This function tests if a king is in check.
}
\details{
The function looks only at squares from which enemy pieces could
attack the king based on the king's current position. If an attacking
enemy piece is found in one of these squares, the function returns \code{TRUE}.
}
\examples{
# Initialize a chess board
position <- bigchess:::position.start()
# Is the white king in check?
is_check_cpp(position, -1)

}
