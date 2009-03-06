\name{rSimple}
\alias{rSimple}
\title{Random numbers with finitely many values}
\description{
Returns a generator of random numbers with finitely many
values. 
}
\usage{
X <- rSimple(table, rep=NULL)
}
\arguments{
  \item{table}{a probability table}
  \item{rep}{sample size}
} 
\details{ 
\code{rSimple} returns a function \code{X(n,drop=TRUE)} which produces random numbers 
with finitely many different values.

If \code{table} is a vector then the components are 
used as values and the random number generator $X$ returns
each value with equal probability. If \code{table} is a matrix
then the first column is used for values and the second column for
probabilities. The second column is normalized (divided by its).
 } 
\author{Helmut Strasser}
\examples{
X <- rSimple(1:6)
X(20)

Y <- rSimple(cbind(1:3,1:3))
Y(5)

Z <- rSimple(cbind(c(-1,0,1),c(0.2,0.4,0.4)))
Z(100)
}
\keyword{distribution}
