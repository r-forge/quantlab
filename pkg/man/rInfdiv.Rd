\name{rInfdiv}
\alias{rNormal}
\alias{rPoisson}
\alias{rGamma}
\title{Infinitely divisible distributions}
\description{
Generating random numbers
from infinitely divisible distributions
}
\usage{
rNormal(k = 1, mean = 0, cov = 1, rep=NULL)

rPoisson(lambda = 1, rgen = NULL, rep=NULL)

rGamma(shape = 1, rate = 1, rep=NULL)

}
\arguments{
  \item{k}{dimension of the variable}
  \item{mean}{mean value or mean vector of the variable}
  \item{cov}{variance or covariance matrix of the variable. If
  \code{k=2} then \code{cov} can be a vector of length 3
  containing variances and correlation.}
  \item{lambda}{intensity of the Poisson variable}
  \item{rgen}{jump distribution of the compound Poisson variable}
  \item{shape}{shape parameter of the gamma variable}
  \item{rate}{rate parameter of the gamma variable}
  \item{rep}{length of random vectors}
}
\value{
These procedures return functions
 \code{X(n,drop=TRUE,t=1)} which produce
random numbers or vectors.

The function \code{X} accepts three variables \code{n},
\code{t} and \code{drop}.

The variable \code{n} defines the dimension of \code{X(n,t)}.
If the random mechanism returns scalars then \code{X(n,t)}
is a vector  of length \code{n}. If the random mechanism
returns vectors of length \code{k} then \code{X(n,t)} is an array
of dimension \code{c(k,n)}.

The variable \code{t} is the exponent of the distribution
in the corresponding convolution semigroup.
}
\author{ Helmut Strasser }
\seealso{ \link{rpLevy} }
\examples{
X = rNormal()
X(4)
Scatter(X(500))

X = rNormal(k=2,cov=c(1,1,0.9))
X(1)
X(5)
Scatter(X(200))

X = rNormal(k=2,cov=c(1,1,0.9))
X(1)
X(5)

X = rPoisson(lambda=10)
X(1)
X(10)

X = rPoisson(lambda=10,rgen=rNormal(k=2))
X(1)
X(10)
Scatter(X(200))

X = rGamma(shape=0.1)
X(1)
X(10)
Hist(X(500),br=30)
}
\keyword{ distribution }
