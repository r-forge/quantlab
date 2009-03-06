\name{rBernoulli}
\alias{rBernoulli}
\alias{rSym}
\title{Bernoulli random numbers}
\description{
Returns generators for steps of discrete random walks.
} 
\usage{
 X <- rBernoulli(p = 0.5, values = c(1, 0), rep=NULL) 

 X <- rSym(k=1, rep=NULL)
} 
\arguments{
  \item{p}{probability of the first value}
  \item{values}{pair of values}
  \item{k}{dimension of random vectors}
  \item{rep}{sample size}
}
\value{
\code{X=rBernoulli()} returns a function \code{X(n,drop=TRUE)}
which produces \code{n} Bernoulli random numbers (if \code{rep==1}) or
\code{n} random samples of size \code{rep}. 

\code{X=rSym()} returns a function \code{X(n,drop=TRUE)} which
produces random vectors being steps of a symmetric random
walk of dimension \code{k}.

Whenever an array is returned then the last dimension 
corresponds to the replications.
}
\seealso{\link{rpRandomwalk}}
\author{Helmut Strasser}
\examples{
X <- rBernoulli()
X(10)
Y <- rBernoulli(values=c(1,-1))
Y(5)
Z <- rBernoulli(p=0.1,values=c(1,3))
Z(5)

X <- rSym()
X(1)
X(20)
X <- rSym(k=2)
X(1)
X(10)
}
\keyword{distribution}
