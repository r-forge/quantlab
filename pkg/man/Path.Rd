\name{PathClass}
\alias{as.path}
\alias{ngrid.default}
\alias{its2path}
\title{Path class}
\description{
Converting objects to objects of class \code{path}.
}
\usage{
as.path(fun = NULL, tlim = c(0,1), time = NULL, ngrid = NULL)

ngrid.default = 500

its2path(y)
}
\arguments{
\item{fun}{
either a function or a string or a vector.

If it is a function (or the name of it) then it expects one
argument and operates elementwise. If it is a string then
it is parsed into an expression defining a function
of one argument named \code{t} and operates elementwise.
If it is a vector then its components are used as function
values of a step function with equidistant steps on the
intervall \code{tlim}.

If \code{is.null(fun)} then \code{fun = function(t) t}.
}
\item{tlim}{
if \code{is.null(time)} or if
\code{is.numeric(fun)} then the time grid is an
equidistant grid of \code{ngrid} points in the
interval \code{tlim}.
}
\item{time}{
a vector containing the points of the time
grid. This variable overwrites \code{tlim} if
\code{!is.numeric(fun)}.}
\item{ngrid}{
the number of points of the time grid
(if \code{is.null(time)}). If \code{is.null(ngrid)}
then \code{ngrid} is taken from the global constant
\code{ngrid.default}}.
\item{y}{object of class \code{its}.}
}
\details{
 A vector \code{p} which is of
\code{class(p)="path"} has a an attribute
\code{attr(p,"time")} which is an (increasingly) ordered
vector of length \code{length(p)}.

An array \code{p} which is of \code{class(p)="path"} has a
an attribute \code{attr(p,"time")} which is an
(increasingly) ordered vector length \code{dim(p)[1]}.

The function \code{as.path} transforms a function or a
vector into a path with a given time attribute.

If \code{is.numeric(fun)} then the function \code{as.path}
distributes the components (columns) of \code{fun} as values of a
right-continuous step function which jumps at equidistant points
of \code{tlim}. The returned path is a discretized version of
this step function.
}
\value{
The value is an object of \code{class="path"}.
}
\author{ Helmut Strasser }
\seealso{\link{pBind}, \link{pClock}, \link{pDelta}, \link{pVariation},
\link{pLag}, \link{pCompress}, \link{pIntegral}, \link{pBracket},
\link{pExp}, \link{pReduce}, \link{pApply}}
\examples{
# Turning a time scale into a path:
as.path(time=0:10)
pPlot(as.path())
pPlot(as.path(tlim=c(0,5)))

# Turning a function graph into a path:
pPlot(as.path(fun="t^2",time=0:10))
pPlot(as.path(fun="pnorm(t)",tlim=c(-4,4)))

# Turning vectors into paths:
Steps(as.path(c(0,1,2,3)))
Steps(as.path(c(0,1,2,3),tlim=c(0,3)))

# Application to random walks:
x = c(0,cumsum(rnorm(300)))
pPlot(as.path(x,tlim=c(0,3)))

X = rpRandomwalk(len=300)
pPlot(as.path(X(1),tlim=c(0,3)))
pPlot(as.path(X(5),tlim=c(0,3)))

pPlot(as.path("sin(2*pi*t)",ngrid=5000),ylim=c(-1,1))
pPlot(pVariation(as.path("sin(2*pi*t)",ngrid=5000),order=1))
pPlot(pBracket(as.path("sin(2*pi*t)",ngrid=5000)),ylim=c(-1,1))
}
\keyword{manip}
\keyword{methods}
