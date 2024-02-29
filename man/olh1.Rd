\name{olh1}
\alias{olh1}
\title{orthogonal Latin hypercube designs}
\description{This function generates an orthogonal Latin hypercube designs upto 6 columns}
\usage{olh1(n, m)}
\arguments{
	\item{n}{number of runs}
	\item{m}{number of columns, between 2 to 6}
	}
\value{
	A orthogonal Latin hypercube design with m columns in n runs
  }
\examples{
olh1(16,3)
}
\author{Baidya Nath Mandal<mandal.stat@gmail.com>}
\keyword{internal}