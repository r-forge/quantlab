\name{rBlackScholes}
\alias{rBS}
\alias{rpBS}
\title{Black-Scholes model}
\description{
Random number generation for the Black-Scholes model.
}
\usage{
rBS(vol = 1, rate = 0, spot = 1, maturity = 1, rep = NULL)

rpBS(vol = 1, rate = 0, spot = 1, tlim = c(0, 1), time = NULL, ngrid = NULL)
}
\arguments{
  \item{vol}{volatility}
  \item{rate}{growth rate of the expected returns}
  \item{spot}{spot price}
  \item{maturity}{maturity}
  \item{rep}{sample size, defaults to 1}
  \item{tlim}{time interval}
  \item{time}{subdivision of the time interval}
  \item{ngrid}{number of intervals in the subdivision}
}
\details{
\code{X=rBS()} returns a function \code{X(n,drop=TRUE)} which
produces random numbers for the Black-Scholes model with fixed maturity.

\code{X=rpBS()} returns a function \code{X(n)} which produces random paths
for the Black-Scholes model.
}
\author{Helmut Strasser}
\seealso{\link{Call.BS}, \link{Put.BS}, \link{call.price.diagram},
\link{put.price.diagram}, \link{Hedge.Call.BS}}
\examples{
X=rBS(); X(20)
mean(X(10000))
X=rBS(rep=5); X(3)

X=rpBS()
pPlot(X(1))
}
\keyword{distribution}
