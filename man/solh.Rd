\name{solh}
\alias{solh}
\title{Sliced orthogonal Latin hypercube designs}
\description{This function generates a sliced orthogonal Latin hypercube designs upto 6 columns}
\usage{solh(n1, t, q)}
\arguments{
	\item{n1}{number of runs in first slice}
	\item{t}{number of slices, currently 3 or 4 are supported}
	\item{q}{number of columns, between 2 to 6}
	}
\value{
	A sliced orthogonal Latin hypercube design with q columns in t(n1-1) + 1 runs in t slices
  }
\examples{
solh(4,5,2)
}
\author{A Anil Kumar<aa9538148952@gmail.com>}
\keyword{sliced}
\keyword{orthogonal}
\keyword{Latin}
\keyword{hypercube}