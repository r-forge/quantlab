\name{rpEquation}
\alias{rpEquation}
\title{Stochastic differential equations}
\description{
This function solves a stochastic differential equation by
Euler iteration.
}
\usage{
rpEquation(start = 1, drift = 0, diffusion = "x", driver = rpWiener())
}
\arguments{
  \item{start}{ initial value (scalar) }
  \item{drift}{ a constant, or a function name or expression string,
  which defines a scalar valued function of the variable \code{x,t} }
  \item{diffusion}{ a constant, or a function name or expression string,
  which defines a scalar valued function of the variable \code{x,t}  }
  \item{driver}{ driving stochastic process }
}
\value{
 The value is a function \code{X(n)} which returns paths
  of the solution of the stochastic differential equation for the time
  grid \code{time}. The variable
  \code{n} determines the number of independent replications which are
  returned.

  The value \code{p} is always of \code{class="path"} which means that it has
  an \code{attr(p,"time")} containing the time grid.
}
\author{ Helmut Strasser }
\seealso{\link{as.path}, \link{rpWiener}}
\examples{
# Stochastic exponential
X = rpEquation(start=1,drift=0,diffusion="x")
pPlot(X(5))

# Ornstein-Uhlenbeck process
W = rpWiener(tlim=c(0,10))
X = rpEquation(start=10,drift="-5*x",diffusion=1,driver=W)
pPlot(X(1))
pPlot(X(5))

# Brownian bridge
X = rpEquation(start=0,drift="-x/(1-t)",diffusion=1)
pPlot(X(10))
}
\keyword{ distribution }
