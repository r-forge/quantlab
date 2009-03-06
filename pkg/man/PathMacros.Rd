\name{PathMacros}
\alias{pBind}
\alias{pClock}
\alias{pDelta}
\alias{pVariation}
\alias{pLag}
\alias{pCompress}
\title{Path utilities}
\description{
Provides basic operations on paths.
}
\usage{
pBind(p,...)

pClock(p)

pDelta(p)

pVariation(p,order=2)

pLag(p)

pCompress(p)
}
\arguments{
  \item{p,...}{ paths }
  \item{order}{ order of variation }
}
\details{
\code{pBind} is an extension of \code{cbind} for objects
of class \code{path}.

\code{pClock} extracts the time attribute of a paths.

\code{pDelta} extends \code{diff} to objects of class
\code{path}.

\code{pVariation} calculates the variation of a path object.

\code{pLag} returns the lagged path.

\code{pCompress} returns path statistics: terminal value, maximum, minimum, average.
}
\value{
  \item{p}{ path }
}
\author{Helmut Strasser}
\seealso{\link{as.path}, \link{its2path}, \link{ngrid.default},
\link{pIntegral}, \link{pBracket}, \link{pExp}, \link{pReduce}, \link{pApply}}
\examples{
p=rpWiener(ngrid=10)()
p
cbind(p)
t=pClock(p); t

q=as.path(sin,time=t)
pBind(p,q)
pDelta(p)
pLag(p)

X=rpWiener(ngrid=5000)
pPlot(pVariation(X(1)))

pPlot(pVariation(as.path("sin(2*pi*t)",ngrid=5000),order=1))

W=rpWiener(ngrid=5000)
pPlot(pVariation(W(1),order=1))
}
\keyword{manip}
