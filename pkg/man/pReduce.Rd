\name{pReduce}
\alias{pReduce}
\title{Matching time scales}
\description{
Reduces the time scale of a path object in order to be consistent to the time
scale of another path object.
}
\usage{
pReduce(p, q=NULL, time=NULL)
}
\arguments{
  \item{p}{Object of class \code{path} with a finer time scale than \code{q}.}
  \item{q}{Object of class \code{path} with a coarser time scale than \code{p}.}
  \item{time}{A time scale coarser than that of \code{p}.}
}
\value{
Returns a modification of \code{p} with the time scale of \code{q}.
}
\author{ Helmut Strasser }
\seealso{\link{as.path}, \link{its2path}, \link{ngrid.default}, \link{pBind}, \link{pClock}, \link{pDelta}, \link{pVariation},
\link{pLag}, \link{pCompress}, \link{pIntegral}, \link{pBracket},
\link{pExp}, \link{pReduce}, \link{pApply}}
\examples{
p=as.path(sin,ngrid=20)
q=as.path(cos,ngrid=10)
p
q
pReduce(p,q)
}
\keyword{manip}
