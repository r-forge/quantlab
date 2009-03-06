\name{EmpStockPrices}
\alias{GetStocks}
\alias{Returns}
\title{Retrieving empirical stock prices}
\description{
Functions for downloading stock prices from the Internet.
}
\usage{
GetStocks(tickers="MSFT", from = c(1, 1, 2003), to = c(31, 1, 2003))
Returns(prices)
}
\arguments{
  \item{tickers}{(character vector)}
  \item{from}{}
  \item{to}{}
  \item{prices}{(class(its))}
}
\value{
\code{GetStocks} returns (daily last) stock prices as objects of \code{class(its)}.
\code{Returns} calculates the log-returns of stock prices as objects of
\code{class(its)}.
}
\author{Helmut Strasser}
\examples{
\dontrun{
y=GetStocks("^dji",from=c(1,1,2004),to=c(31,12,2004))
print(y)
pPLot(y)
pPLot(Returns(y))

y=GetStocks(c("msft","nok"),from=c(1,1,2004),to=c(31,12,2004))
pPLot(y)
pPlots(Returns(y))
}
}
\keyword{misc}
