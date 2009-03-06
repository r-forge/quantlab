\name{Discrete Hedging}
\alias{Hedge.Call.BS}
\title{Discrete Hedging}
\description{
This topic is concerned with
MC-simulation of discrete hedging. At the moment only the case of a vanilla call 
under the Black-Scholes model is covered.
}
\usage{
Hedge.Call.BS(strike = 1, vol = 1, rate = 0, rlrate = 0, start = 1, horizon = 1)
}
\arguments{
  \item{strike}{strike price}
  \item{vol}{volatility (per year) of the underlying}
  \item{rate}{growth rate of expected returns of the underlying}
  \item{rlrate}{riskless rate}
  \item{start}{splot price of the underlying}
  \item{horizon}{time horizon of the contract}
}
\details{
The command returns a function

\code{ff=function(n=1,rebalance=100,plot=FALSE)}

which can be used for two purposes. If \code{n=1} or \code{plot=TRUE}
the function returns details of hedging a single path 
of the underlying. Otherwise \code{n} determines the number of paths for which the
terminal error of the hedging process is returned.

The parameter \code{rebalance} determines the number of rebalance steps used for 
hedging.
}
\author{Helmut Strasser}
\examples{
X=Hedge.Call.BS(vol=0.2,rate=0.08,rlrate=0.04)
X()
X(plot=TRUE)
Hist(X(n=1000))
}
\keyword{misc}

