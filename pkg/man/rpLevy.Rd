\name{rpLevy}
\alias{rpLevy}
\alias{rpWiener}
\alias{rpPoisson}
\alias{rpGamma}
\title{Levy processes}
\description{
These procedures return functions which generate random paths
from Levy processes.
}
\usage{
rpLevy(rdist = rNormal(), tlim = c(0, 1), time = NULL, ngrid = NULL)

rpWiener(k = 1, mean = 0, cov = 1, tlim = c(0, 1), time = NULL, ngrid = NULL)

rpPoisson(lambda = 1, rgen = NULL, tlim = c(0, 1),time = NULL, ngrid = NULL)

rpGamma(shape =1, rate = 1, tlim = c(0, 1), time = NULL, ngrid = NULL)
}
\arguments{
\item{rdist}{the infinitely divisible distribution defining the Levy
process.}
\item{k}{dimension of the process.}
\item{mean}{mean value or mean vector of the process.}
\item{cov}{variance or covariance matrix of the process}. If
\code{k=2} {then \code{cov} can be a vector of length 3
containing variances and correlation.}
\item{lambda}{intensity of the Poisson process.}
\item{rgen}{jump distribution of the compound Poisson process.}
\item{shape}{shape parameter of the Gamma process.}
\item{rate}{rate parameter of the Gamma process.}
\item{time}{a vector containing the point of the time grid.}
\item{tlim}{if \code{is.null(time)} then the time grid is
an equidistant grid of \code{ngrid} points in the \code{interval}.
}
\item{ngrid}{the number of points of the time grid (if \code{is.null(time)}).
If \code{is.null(ngrid)} then \code{ngrid} is taken from the globel constant
\code{ngrid.default}}
}
\details{
  A Levy process is a stochastic process with stationary and independent
  increments. Its distribution is determined by the family of distributions of
  increments which depends on one parameter corresponding to the length of
  the underlying time interval.

  In order to specify a Levy process the family of (infinitely divisible)
  distributions of the increments has to be specified. At present the
  following distributions are prepared for defining Levy processes:

  \code{rNormal(k,mean,cov)}: univariate or multivariate normal distributions

  \code{rPoisson(lambda,rgen)}: univariate or multivariate compound Poisson distributions

  \code{rGamma(shape,rate)}: univariate Gamma distributions

  The functions \code{rpWiener}, \code{rpPoisson}, \code{rpGamma} are
  convenience functions based on \code{rpLevy}.
}
\value{
  The procedures return functions \code{X(n)} which produce random paths of
  stochastic processes.

  \code{X} is a function which returns random paths
  of the Levy process for the time grid \code{time}. The variable
  \code{n} determines the number of independent replications which
  are returned.

  \code{p=X(n)} is an array whose first dimension
  contains the paths and whose last dimension contains the
  replications. There can be a third dimension (between the first
  and the last) corresponding to the components of vector valued
  infinitely divisible distribution.

  Dimensions of size one are dropped.

  Note that specifying \code{time} allows to construct random number
  generators of single values of Levy processes.
}
\author{Helmut Strasser}
\seealso{ \link{as.path}, \link{rNormal}, \link{rPoisson}, \link{rGamma}}
\examples{
# Wiener process
X=rpWiener(k=1,tlim=c(0,5))
pPlot(X(10))
pPlot(pDelta(X(1)))
pPlot(pVariation(X(1)))

X=rpWiener(k=1,mean=1)
pPlot(X(1))

X=rpWiener(k=2)
pPlot(X(1),"parametric")

# Levy paths for
X=rpLevy(time=3,rdist=rNormal())
X(20)
X=rpLevy(time=c(0,1,2,3),rdist=rNormal())
X(10)
X=rpLevy(time=1,rdist=rNormal(k=3))
X(5)
X=rpLevy(time=c(0,1,2,3),rdist=rNormal(k=3))
X(5)
X=rpLevy(time=c(0,1,2,3),rdist=rNormal(k=2,mean=c(1,2),cov=c(2,4,0.5)))
X(5)

X=rpLevy(tlim=c(0,3),rdist=rNormal())
pPlot(X(1))
pPlot(X(5))
X=rpLevy(rdist=rNormal(k=2))
pPlot(X(1))
pPlot(X(1),mode="parametric")

X=rpWiener(time=seq(0,1,0.01),mean=10)
pPlot(X(1))

X=rpPoisson(tlim=c(0,10),lambda=2,rgen=NULL)
Steps(X(1))
pPlot(X(10))
X=rpPoisson(tlim=c(0,10),lambda=2,rgen=rSym())
Steps(X(1))
X=rpPoisson(tlim=c(0,10),lambda=10,rgen=rSym(2))
pPlot(X(1),mode="parametric")
X=rpPoisson(tlim=c(0,10),lambda=10,rgen=rNormal())
pPlot(X(5))

X=rpPoisson(time=1,lambda=5,rgen=rNormal())
Normalplot(X)

X=rpGamma(shape=10)
pPlot(X(5))
X=rpGamma(shape=100)
pPlot(X(5))
}
\keyword{distribution}
