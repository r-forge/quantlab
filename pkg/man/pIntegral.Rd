\name{pIntegral}
\alias{pIntegral}
\title{ Integrals as paths }
\description{
This function returns indefinite (stochastic)
integrals as objects of class path.
}
\usage{
pIntegral(fun = "x", driver = NULL, y = NULL, tlim = c(0,1),
       ngrid = NULL, v = 1, adjust = "left", last = FALSE)
}
\arguments{
  \item{fun}{ function name or expression string, which defines
  a scalar valued function of the scalar variables \code{x,t} }
  \item{driver}{ integrator (of class path). If \code{is.null(driver)}
  then the integral is a Lebesgue integral calculated on
  an equidistant grid of \code{ngrid} points }
  \item{y}{ additional input for the expression string}
  \item{tlim}{ limits of the time interval. Is only used if
  \code{is.null(driver)} }
  \item{ngrid}{ the number of points of the time grid (if \code{is.null(time)}).
  If \code{is.null(ngrid)} then \code{ngrid} is taken from the globel constant
  \code{ngrid.default}. Is only used if \code{is.null(driver)} }
  \item{v}{ if \code{(v==1)} then the driving path is used as
  measure defining function. If \code{(v==2)} then the quadratic variation
  of the driving path is used as measure defining function.
  }
  \item{adjust}{ defines the adjustment of the Riemannian sums
  used for calculating the integrals. Possible values are
  \code{"left"}, \code{"right"} and \code{"strat"}
  (Stratonovich integrals). }
  \item{last}{only the terminal values is returned}
}
\details{
  Integrals as calculated as Riemannian sums.
}
\value{
  The value \code{p} is an object of \code{class(p)="path"}.
}
\author{ Helmut Strasser }
\seealso{\link{as.path}, \link{its2path}, \link{ngrid.default}, \link{pBind}, \link{pClock}, \link{pDelta}, \link{pVariation},
\link{pLag}, \link{pCompress}, \link{pBracket}, \link{pExp}, \link{pReduce}, \link{pApply}}
\examples{
# Simple Lebesgue integrals:
p = pIntegral("sin(t)", tlim=c(-5,5))
pPlot(p)
p = pIntegral("1/t", tlim=c(1,10))
pPlot(p)

# Stieltjes integrals:
driver = as.path("pnorm(t)",tlim=c(-5,5))
p = pIntegral("t",driver)
pPlot(p)
q = pIntegral("t*dnorm(t)",tlim=c(-5,5))
pPlot(pBind(p,q))

driver = as.path("pnorm(t)",tlim=c(-5,5),ngrid=10000)
p = pIntegral("t",driver)
q = pIntegral("t*dnorm(t)",tlim=c(-5,5),ngrid=10000)
pPlot(pBind(p,q))

# Wiener integrals:
W = rpWiener(tlim=c(0,1))
p = pIntegral("t",W(1))
pPlot(p)
pPlot(pVariation(p))
pPlot(pIntegral("t",W(5)))

# Ito integrals:
W = rpWiener(tlim=c(0,1))
p = pIntegral("x",W(1))
pPlot(pVariation(p))
pPlot(pIntegral("x",W(5)))
pPlot(pVariation(pIntegral("x",W(20))))

p = W(1)
q1 = pIntegral("x",p)
q2 = (p*p-pClock(p))/2
pPlot(pBind(q1,q2))

W = rpWiener(tlim=c(0,1),ngrid=10000)
p = W(1)
q1 = pIntegral("x",p)
q2 = (p*p-pClock(p))/2
pPlot(q1-q2)

# Stratonovich integral
q1 = pIntegral("x",p,adjust="right")
q2 = (p*p-pClock(p))/2
pPlot(q1-q2)

q1 = pIntegral("x",p,adjust="strat")
q2 = p*p/2
pPlot(q1-q2)

q1 = pIntegral("x",p,adjust="left")
q2 = p*p/2
pPlot(q1-q2)

# Ito formula


}
\keyword{manip}
