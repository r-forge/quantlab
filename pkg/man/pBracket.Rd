\name{pBracket}
\alias{pBracket}
\title{ Quadratic covariation of paths }
\description{
The function calculates the quadratic covariation of two
paths.
}
\usage{
r = pBracket(p, q = NULL)
}
\arguments{
  \item{p}{ object of \code{class("path") }}
  \item{q}{ object of \code{class("path") }}
}
\value{
  The function returns the covariation as an object of \code{class("path")}.
}
\author{ Helmut Strasser }
\seealso{\link{as.path}, \link{its2path}, \link{ngrid.default}, \link{pBind}, \link{pClock}, \link{pDelta}, \link{pVariation},
\link{pLag}, \link{pCompress}, \link{pIntegral}, \link{pExp}, \link{pReduce}, \link{pApply}}
\examples{
# Quadratic variation:
W = rpWiener()
pPlot(pBracket(W(1)))
W = rpWiener(ngrid=10000)
pPlot(pBracket(W(1)))

X = rpPoisson(lambda=10)
pPlot(pBracket(X(1)))

p = as.path("sin(20*t)",ngrid=10000)
pPlot(p)
pPlot(pBracket(p),ylim=c(-1,1))

# Quadratic covariation:
p = W(1)
q = W(1)
pPlot(pBracket(p,q),ylim=c(-1,1))

p = rpWiener(ngrid=10000)(1)
q = rpPoisson(lambda=10,ngrid=10000)(1)
pPlot(pBracket(p,q),ylim=c(-1,1))
}
\keyword{manip}
