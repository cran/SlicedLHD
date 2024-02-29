\name{om16r}
\alias{om16r}
\title{Orthogonal matrix for 16r runs}
\description{This function generates an orthogonal matrix in 16r runs with 6 columns with given numbers}
\usage{om16r(n,r,levels)}
\arguments{
	\item{n}{number of runs}
	\item{r}{a positive integer}
	\item{levels}{a vector containing 16r numbers}
	}
\value{
	\item{design}{(An orthogonal matrix of 16 runs and 6 columns}
  }
\examples{
om16r(16,2,1:32)
}
\author{Baidya Nath Mandal<mandal.stat@gmail.com>}
\keyword{internal}