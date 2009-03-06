\name{BinomialTrees}
\alias{BinomTree}
\alias{CRR}
\title{Binomial trees}
\description{
Calculates a binomial tree for a financial claim.
}
\usage{
BinomTree(n, up, down = 1/up, rate, spot = 1, claim=Call(1), mode = "european")
CRR(n,vol=1,rate=0,spot=1,maturity=1,claim=Call(1),mode="european")
}
\arguments{
  \item{n}{number of steps}
  \item{up}{size of upward step (multiplicative)}
  \item{down}{size of downward step (multiplicative)}
  \item{vol}{volatility}
  \item{rate}{riskless rate}
  \item{spot}{spot price}
  \item{claim}{payoff (at present only \code{Call} and \code{Put} are supported)}
  \item{maturity}{maturity}
  \item{mode}{exercise mode, "european" or "american"}
}
\details{
\code{BinomTree} is the low-level function. \code{CRR} is a convenience function
providing the Cox-Ross-Rubinstein approximation of the Black-Scholes model.
}
\value{
  The function prints the initial value of the claim and returns an invisible list
  with components
  \item{Stock}{stock price}
  \item{Value}{portfolio value}
}
\author{Helmut Strasser}
\seealso{\link{Call}, \link{Put}}
\examples{
t=BinomTree(n=5,up=1.06,rate=0.04,claim=Call(1))
lapply(t$Stock,round,3)
lapply(t$Value,round,3)

system.time(BinomTree(n=500,up=1.06,rate=0.04,claim=Call(1)))

BinomTree(n=30,up=1.06,rate=0.04)
BinomTree(n=30,up=1.06,rate=0.04,mode="am")
BinomTree(n=30,up=1.06,rate=0.04,claim=Put(1))
BinomTree(n=30,up=1.06,rate=0.04,claim=Put(1),mode="am")

CRR(n=100,vol=0.2,rate=0.04,spot=1,maturity=1)
}
\keyword{misc}
