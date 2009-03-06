\name{rpRandomwalk}
\alias{rpRandomwalk}
\title{Random Walk}
\description{
Returns random numbers of random walks.
}
\usage{
rpRandomwalk(len = 100, rgen = rSym(), start=0)
}
\arguments{
  \item{len}{length of generated vectors}
  \item{rgen}{random number generator}
  \item{start}{initial value}
}
\value{
\code{rpRandomwalk} returns a function {X(n)}. \code{X()}
or \code{X(1)} is a vector of \code{length(x)=len+1} starting with
\code{x[1]=0} and \code{x[2:len+1]} containing the values of
the random walk (cumulative sum of i.i.d. realizations of
\code{rgen}).

If \code{n>1} then \code{X(n)} is a matrix with \code{n}
columns containing independent replications of the
random walk.
}
\author{Helmut Strasser}
\seealso{\link{rBernoulli}, \link{rSym}}
\examples{
X=rpRandomwalk(len=1)
X(1)
X(5)
X(1,drop=FALSE)
X(5,drop=FALSE)

X=rpRandomwalk(len=20)
X(1)
Steps(X(1))
X(3)

X=rpRandomwalk(len=1,rgen=rSym(k=2))
X(1)
X(5)

X=rpRandomwalk(len=10,rgen=rSym(k=2))
X(1)
X(3)

X=rpRandomwalk(len=300,rgen=rSym(k=2))
plot(X(1),type="l",ann=FALSE)
\dontrun{Movie(X(1))}

#Arcsin-Law:
X=rpRandomwalk()
argmax=function(x){match(max(x),x)}
x=apply(X(1000),2,argmax)
Hist(x)
Ecdf(x)
}
\keyword{distribution}
