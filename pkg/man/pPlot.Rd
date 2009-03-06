\name{pPlot}
\alias{pPlot}
\title{Diagrams of vectors, arrays and paths}
\description{
This function replaces the plot-function for \code{path}-objects.
}
\usage{
pPlot(x, mode = "parallel", ...)
}
\arguments{
  \item{x}{vector, array or path}
  \item{mode}{"parallel" or "parametric"}
  \item{\dots}{additional graphics parameters}
}
\author{Helmut Strasser}
\examples{
W=rpWiener(k=2)
pPlot(W(1))
pPlot(W(1),mode="par")

pPlot(1:10)
pPlot(rnorm(100))
\dontrun{xx=get.stocks("MSFT");pPlot(xx)}

pPlot(matrix(rnorm(200),100,2))
pPlot(matrix(rnorm(200),100,2),mode="par")

W=rpWiener()
pPlot(pIntegral(driver=W(1)))
}
\keyword{hplot}
