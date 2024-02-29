\name{slh}
\alias{slh}
\title{Sliced  Latin hypercube designs}
\description{This function generates a sliced Latin hypercube designs}
\usage{slh(n1, t, q)}
\arguments{
	\item{n1}{number of runs in first slice}
	\item{t}{number of slices, currently 3 or 4 are supported}
	\item{q}{number of columns, between 2 to 6}
	}
\value{
	A sliced Latin hypercube design with q columns in t(n1-1) + 1 runs in t slices
  }
\examples{
slh(5,3,4)
}
\author{A Anil Kumar<aa9538148952@gmail.com>}
\keyword{sliced}
\keyword{Latin}
\keyword{hypercube}