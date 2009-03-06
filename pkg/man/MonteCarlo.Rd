\name{MonteCarlo}
\alias{MC}
\title{Monte Carlo Simulation}
\description{
This topic is concerned with Monte Carlo simulation of expectations. It provides tools for
computation with limited memory (blockwise calculation). At the moment there is no
C-implementation.
}
\usage{
MC(rgen,iter=100000,blocks=10000,silent=FALSE)
}
\arguments{
  \item{rgen}{(function): random number generator}
  \item{iter}{(integer): number of iterations}
  \item{blocks}{(integer): size of blocks}
  \item{silent}{(Bollean)}
}
\value{
Returns the sample mean of \code{fun} applied to \code{rgen}.
}
\note{The performance is considerably slower than calculation within a single vector.}
\author{Helmut Strasser}
\examples{
MC(rNormal(k=1),iter=100,blocks=10)
MC(rNormal(k=2),iter=100,blocks=10)

MC(rBS(),iter=100,blocks=10)
}
\keyword{distribution}
