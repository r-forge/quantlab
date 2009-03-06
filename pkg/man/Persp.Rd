\name{Persp}
\alias{Persp}
\title{Interactive surface plotting}
\description{
A perspective surface plot which can be moved by interaction.
}
\usage{
Persp(x,y=NULL,z=NULL,xlim=c(-3,3),ylim=c(-3,3),ngrid=30,zlim=NULL,
         phi=10,theta=30,pal=terrain.colors,control=TRUE,...)
}
\arguments{
  \item{x}{numeric vector or a string. If \code{x} is a numeric vector then it
  has to be ordered (increasing) and is used as set of x-coordinates. In this
  case both \code{y} and \code{z} have to be specified. If \code{x} is a string containing an functional expression
  with variables x and y then it is used for defining a function whose graph
  is plotted.}
  \item{y}{numeric vector. Is used as set of y-coordinates if \code{x} is a
  numeric vector. }
  \item{z}{numeric matrix of dimension \code{dim(z)=c(length(x),length(y))}. Contains
  the values of the plotted function.}
  \item{xlim}{interval of x-coordionates if \code{x} is a string.}
  \item{ylim}{interval of y-coordinates if \code{x} is a string.}
  \item{zlim}{optional user defined interval of z-coordinates to be used for plotting.}
  \item{ngrid}{number of points in the subdivision of x-coordinates and y-coordinates
  if \code{x} is a string.}
  \item{phi}{starting value of viewpoint angle.}
  \item{theta}{starting value of viewpoint angle.}
  \item{pal}{color palette}
  \item{control}{Boolean. If TRUE then the viewpoint can be varied interactively by means
  of the arrow keys (type END for stopping interaction).}
  \item{...}{additional graphics parameters.}
}
\author{Helmut Strasser}
\examples{
\dontrun{
Persp("x^2-y^2")
}
}
\keyword{hplot}
\keyword{iplot}
