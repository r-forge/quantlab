\name{Payoff_functions}
\alias{Call}
\alias{Put}
\alias{BinCall}
\alias{BinPut}
\alias{KnockInCall}
\alias{KnockOutCall}
\alias{AverageStrikeCall}
\alias{AverageRateCall}
\alias{FloatingStrikePut}
\title{Payoff functions}
\description{
Several payoff functions of financial options.
}
\usage{
Call(strike = 1)
Put(strike = 1)
BinCall(strike = 1)
BinPut(strike = 1)
KnockInCall(strike = 1,barrier = 1.5)
KnockOutCall(strike = 1,barrier = 1.5)
AverageStrikeCall(a,b)
AverageRateCall(a,b)
FloatingStrikePut(a=1)
}
\arguments{
  \item{strike}{vector of strike prices}
  \item{barrier}{barrier}
  \item{a}{parameters (see references)}
  \item{b}{parameters (see references)}
}
\references{
[1] Helmut Strasser. Advanced Financial Mathematics and Structured Derivatives.
WU Executive Academy, Lecture Notes, May 2008.
}
\author{Helmut Strasser}
\examples{
ff=Call(5)
x=1:10; ff(x)
x=cbind(1:10,2:11); ff(x)

ff=Call(c(3,7))
x=1:10; ff(x)
x=cbind(1:10,2:11,3:12); ff(x)

ff=Put(5)
x=1:10; ff(x)
x=cbind(1:10,2:11); ff(x)

ff=Put(c(3,7))
x=1:10; ff(x)
x=cbind(1:10,2:11,3:12); ff(x)

ff=BinCall(5)
x=1:10; ff(x)
x=cbind(1:10,2:11); ff(x)

ff=BinCall(c(3,7))
x=1:10; ff(x)
x=cbind(1:10,2:11,3:12); ff(x)

ff=BinPut(5)
x=1:10; ff(x)
x=cbind(1:10,2:11); ff(x)

ff=BinPut(c(3,7))
x=1:10; ff(x)
x=cbind(1:10,2:11,3:12); ff(x)
}
\keyword{misc}
