\name{BlackScholesPrices}
\alias{Call.BS}
\alias{Put.BS}
\alias{call.price.diagram}
\alias{put.price.diagram}
\title{Closed form prices for Black-Scholes}
\description{
This topic is concerned with pricing tools for the Black-Scholes model.
At the moment only vanilla calls and puts are covered.
}
\usage{
Call.BS(vol = 1, strike = 1, rate = 0, spot = 1, maturity = 1)

Put.BS(vol = 1, strike = 1, rate = 0, spot = 1, maturity = 1)

call.price.diagram(vol = 1, strike = 1, rate = 0, maturity = 1, disp = "Value")

put.price.diagram(vol = 1, strike = 1, rate = 0, maturity = 1, disp = "Value")
}
\arguments{
  \item{vol}{volatility}
  \item{strike}{strike price}
  \item{rate}{riskless rate}
  \item{spot}{spot price}
  \item{maturity}{maturity}
  \item{disp}{plot variable ("Value", "Delta", "Gamma"}
}
\details{
\code{Call.BS} and \code{Put.BS} return a list with components Value, Delta and
Gamma, calculated for the price of a European call option.

The functions \code{call.price.diagram} and \code{put.price.diagram}
provide the corresponding diagrams. The parameters can be varied by interactive
keyboard input.
}
\value{
\code{Call.BS} and \code{Put.BS} return
  \item{Value}{option price}
  \item{Delta}{option delta}
  \item{Gamma}{option gamma}
}
\author{Helmut Strasser}
\seealso{\link{rBS}, \link{rpBS}, \link{Hedge.Call.BS}}
\examples{
pf=function(x,y) Call.BS(spot=x,vol=y)$Value
Persp("pf(x,y)",xlim=c(0,2),ylim=c(0,1),xlab="spot",ylab="vol",zlab="price",
   control=FALSE)

Call.BS(strike=seq(from=0.5,to=1.5,by=0.1))$Value
}
\keyword{misc}
