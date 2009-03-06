\name{pApply}
\alias{pApply}
\title{Application of functions to paths}
\description{
Applies a function to a path object.
}
\usage{
pApply(fun = "x", p = NULL, tlim = c(0, 1), ngrid = NULL)
}
\arguments{
  \item{fun}{A string, defining a function of \code{x} and/or \code{t},
  or the name of a function \code{function(x,t)}}.
  \item{p}{A path object which can be used as argument \code{x} of
  the function argument.}
  \item{tlim}{Limits of the time interval (used if \code{p=NULL}).}
  \item{ngrid}{Length of the subdivision (used if \code{p=NULL})}
}
\value{
  Returns an object of class \code{path}.
}
\author{ Helmut Strasser }
\seealso{\link{as.path}, \link{its2path}, \link{ngrid.default}, \link{pBind}, \link{pClock}, \link{pDelta}, \link{pVariation},
\link{pLag}, \link{pCompress}, \link{pIntegral}, \link{pBracket}, \link{pReduce}, \link{pExp}}
\examples{
W=rpWiener(ngrid=5000)
pPlot(pApply("x+sin(2*pi*t)",W(1)))
pPlot(pBracket(pApply("x+sin(2*pi*t)",W(1))))
}
\keyword{manip}
