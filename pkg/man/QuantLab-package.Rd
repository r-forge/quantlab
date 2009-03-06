\name{QuantLab-package}
\alias{QuantLab-package}
\alias{QuantLab}
\docType{package}
\title{
Package Quantlab: Overview
}
\description{
The package QuantLab contains a collection of R-commands which support computer experiments
in the fields of probability, stochastic processes, stochastic analysis and 
mathematical finance.

The package is UNDER CONSTRUCTION. 
}
\details{
\tabular{ll}{
Package: \tab QuantLab\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2008-06-08\cr
Depends: \tab R (>= 2.6.1), its, abind, Hmisc\cr
License: \tab GPL\cr
}
}
\section{Random numbers}{
The package provides some random number generators.

The common feature of all random number generators \code{X=rgen(...)} 
provided by this package is that they return functions 
\code{X(n,drop=TRUE)}. The model parameters are defined as 
variables of the generators, the number of replications is defined as a variable
of the returned function. 

\tabular{ll}{
\link{rBernoulli} \tab Bernoulli random numbers \cr
\link{rSimple} \tab categorial random variables \cr
\link{rSym} \tab random unit vectors \cr  
\link{rNormal} \tab normal random variables \cr
\link{rPoisson} \tab compound Poisson random variables \cr
\link{rGamma} \tab gamma random variables \cr
\link{rBS} \tab lognormal random variables (Black-Scholes model)
}}
\section{Random sequences}{

\tabular{ll}{
\link{rpRandomwalk} \tab generating sequences of random walks \cr
}}
\section{Tools for paths}{
The central object of the package are the simulation and analysis of 
random paths in continuous time. For this purpose the (S3)-class \emph{path}
is defined. The following are basic operations for objects of class \emph{path}.

\tabular{ll}{
\link{as.path}\tab converting tool \cr
\link{its2path} \tab converting \emph{its}-objects to \emph{path}-objects \cr
\link{pClock}\tab extracting the time scale \cr
\link{pBind}\tab binding paths together \cr
\link{pReduce}\tab synchronizing paths \cr 
\link{pCompress}\tab extracting path statistics \cr
\link{pDelta}\tab differences \cr
\link{pLag}\tab lags \cr
\link{pVariation}\tab variations of any order \cr
\link{pBracket}\tab quadratic variation \cr
\link{pIntegral}\tab stochastic integration (including Stieltjes integration) \cr
\link{pExp}\tab stochastic exponential (of continuous paths) \cr
\link{pApply}\tab applying functions for path transformation
}}
\section{Random processes}{
The package provides generators of random paths for continuous time.

Each generator \code{X=rgen(...)} returns a function \code{X(n)}. The model 
parameters are defined as variables of the generators, the number of replications 
is defined as a variable of the returned function. 

\tabular{ll}{
\link{rpWiener} \tab Wiener process \cr
\link{rpPoisson}\tab compound poisson process \cr
\link{rpGamma}\tab gamma process\cr
\link{rpLevy} \tab Levy process (based on an infinitely divisible distribution) \cr
\link{rpBS} \tab Black-Scholes process \cr
\link{rpEquation} \tab Ito-equation process
}}
\section{Statistics}{
The package provides some convenience functions for quick explorative
data analysis.

\tabular{ll}{
\link{Barplot}\tab a simple barplot \cr
\link{Boxplot}\tab a simple boxplot \cr
\link{Hist}\tab a correct histogram \cr
\link{Ecdf}\tab empirical distribution function \cr
\link{Scatter}\tab a scatter plot \cr
\link{Normalplot}\tab a normalplot \cr
\link{Shape}\tab skewness and kurtosis
}}
\section{Finance}{
At the moment the package provides a few elementary pricing tools.

Payoff functions:
\tabular{ll}{
\link{Call}\tab vanilla call\cr
\link{Put}\tab vanilla put \cr
\link{BinCall}\tab binary call \cr
\link{BinPut}\tab binary put \cr
\link{KnockOutCall} \tab knock-out call\cr
\link{KnockInCall} \tab knock-in call \cr
\link{AverageStrikeCall} \tab asian call with floating strike \cr
\link{AverageRateCall} \tab asian call with fixed strike \cr
\link{FloatingStrikePut} \tab lookback put with maximum as floating strike
}
Closed form pricing:
\tabular{ll}{
\link{Call.BS}\tab prices and greeks of a European call (Black-Scholes) \cr
\link{Put.BS}\tab prices of a European put (Black Scholes) \cr
\link{call.price.diagram}\tab interactive plot for call prices and greeks (Black-Scholes) \cr
\link{put.price.diagram}\tab interactive plot for put prices (Black-Scholes)
}
Binomial trees:
\tabular{ll}{
\link{BinomTree}\tab binomial trees (backwards pricing, early exercise included) \cr
\link{CRR}\tab binomial approximation of Black-Scholes model (Cox-Ross-Rubinstein)
}
Discrete Hedging:
\tabular{ll}{
\link{Hedge.Call.BS} \tab simulation of discrete hedging
}}
\section{Retrieving data}{

\tabular{ll}{
\link{GetStocks}\tab historical stock prices from yahoo.finance \cr
\link{Returns}\tab transforming stock prices into log-returns
}}
\section{Plotting}{
The package provides some convenience functions for plotting.

\tabular{ll}{
\link{pPlot}\tab static function graphs of paths \cr
\link{Persp}\tab perspective surface plots (interactive aspect variation)\cr
\link{Steps}\tab graphs of step functions
}}
\section{Simulation}{

\tabular{ll}{
\link{MC} \tab Monte Carlo simulation of expectations
}}
\author{
Helmut Strasser

Maintainer: Helmut Strasser <Helmut.Strasser@gmx.com>
}
\references{
[1] Rama Cont and Peter Tankov. Financial Modelling with Jump Processes. Chapman &
Hall/CRC, 1 edition, December 2003.

[2] Timothy Falcon Crack. Heard on the Street: Quantitative Questions from Wall Street Job
Interviews. Timothy Crack, 10 revised edition, 2007.

[3] John C. Hull. Options, futures, and other derivatives. 4th ed. Prentice-Hall International
Editions., 2000.

[4] Steven E. Shreve. Stochastic Calculus for Finance I: The Binomial Asset Pricing Model.
Springer, 1 edition, April 2004.

[5] Steven E. Shreve. Stochastic Calculus for Finance II: Continuous-Time Models. Springer,
1 edition, June 2008.

[6] Helmut Strasser. Introduction to Probability Theory and Stochastic Processes. 
Vienna Graduate School of Finance, Lecture Notes, November 2007.

[7] Helmut Strasser. Advanced Financial Mathematics and Structured Derivatives.
WU Executive Academy, Lecture Notes, May 2008.

[8] Paul Wilmott. Paul Wilmott on Quantitative Finance 2 Volume Set. Wiley, 2000.
}
\keyword{ package }
\examples{
\dontrun{
# Retrieving empirical stock prices
y=GetStocks(c("msft","nok"),from=c(1,1,2004),to=c(31,12,2004))
pPlot(y)
pPlot(Returns(y))

# MC simulation of stock prices
X=rpBS(rate=0.04,vol=0.2)
pPlot(X(20))

x=apply(X(500),1,mean)
plot(attr(X(),"time"),x,type="l")

# Pricing of claims
r=0.04
S=rBS(vol=0.2,rate=r,maturity=1,spot=1)
mean(Call(1)(S(1000000)))*exp(-r)
Call.BS(vol=0.2,rate=r)$Value

# Price functions
S=rBS(vol=0.2,rate=0.04)
sp=seq(from=0.5,to=1.5,length=50)
pr=sp*apply(Call(1/sp)(S(50000)),2,mean)*exp(-0.04)

sm=smooth.spline(sp,pr,spar=0.5)
price=function(x) predict(sm,x)$y
plot(price,0.5,1.5)
pr0=Call.BS(spot=sp,vol=0.2,rate=0.04)$Value
lines(sp,pr0,col="red")

# Interactive diagrams (use arrow keys)
call.price.diagram(rate=0.05)
put.price.diagram(rate=0.05)

pf=function(x,y) Call.BS(spot=x,vol=y)$Value
Persp("pf(x,y)",xlim=c(0,2),ylim=c(0,1),xlab="spot",
   ylab="vol",zlab="value")

# Greeks
S=rBS(vol=0.2,rate=0.04)
sp=seq(from=0.5,to=1.5,length=50)
pr=sp*apply(Call(1/sp)(S(10000)),2,mean)*exp(-0.04)
sm=smooth.spline(sp,pr,spar=0.5)

delta=function(x) predict(sm,x,deriv=1)$y
plot(delta,0.5,1.5)
dl0=Call.BS(spot=sp,vol=0.2,rate=0.04)$Delta
lines(sp,dl0,col="red")

gamma=function(x) predict(sm,x,deriv=2)$y
plot(gamma,0.5,1.5)
gm0=Call.BS(spot=sp,vol=0.2,rate=0.04)$Gamma
lines(sp,gm0,col="red")

pf=function(x,y) Call.BS(spot=x,vol=y)$Delta
Persp("pf(x,y)",xlim=c(0,2),ylim=c(0,1),xlab="spot",
   ylab="vol",zlab="delta")

pf=function(x,y) Call.BS(spot=x,vol=y)$Gamma
Persp("pf(x,y)",xlim=c(0,2),ylim=c(0,1),xlab="spot",
   ylab="vol",zlab="gamma")

# Binomial trees
t=BinomTree(n=5,up=1.06,rate=0.04,claim=Call(1))
lapply(t$Stock,round,3)
lapply(t$Value,round,3)

BinomTree(n=30,up=1.06,rate=0.04,claim=Call(1))
BinomTree(n=30,up=1.06,rate=0.04,claim=Call(1),mode="am")

BinomTree(n=30,up=1.06,rate=0.04,claim=Put(1))
BinomTree(n=30,up=1.06,rate=0.04,claim=Put(1),mode="am")

Call.BS(vol=0.2,strike=1,rate=0.04,spot=1,maturity=1)$Value
CRR(n=100,vol=0.2,rate=0.04,spot=1,maturity=1,claim=Call(1))
CRR(n=1000,vol=0.2,rate=0.04,spot=1,maturity=1,claim=Call(1))

# Paths of processes
pPlot(rpWiener()(1))
pPlot(rpWiener()(20))

X=rpWiener(mean=1,cov=0.1)
pPlot(X(1))

pPlot(rpBS()(1))
pPlot(rpBS()(20))
pPlot(rpBS(vol=0.2,rate=0.08)(20))

# Variation of paths
pPlot(as.path("sin(2*pi*t)",ngrid=5000),ylim=c(-1,1))
pPlot(pVariation(as.path("sin(2*pi*t)",ngrid=5000),order=1))
pPlot(pBracket(as.path("sin(2*pi*t)",ngrid=5000)),ylim=c(-1,1))

W=rpWiener(ngrid=5000)
pPlot(W(1))
pPlot(pVariation(W(1),order=1))
pPlot(pBracket(W(1)))

pPlot(pApply("x+sin(2*pi*t)",W(1)))
pPlot(pBracket(pApply("x+sin(2*pi*t)",W(1))))

# Concept of integrals
fun=function(t) exp(-t^2)
integrate(fun,-2,3)

p1=pIntegral("exp(-t^2)",tlim=c(-2,3),ngrid=20)
p1[length(p1)]
p2=pIntegral("exp(-t^2)",tlim=c(-2,3),ngrid=200)
p2[length(p2)]
pPlot(pBind(p1,pReduce(p2,p1)))

n=seq(from=50,to=1000,by=50)
i=numeric(0)
for (k in n)
   i=c(i,pIntegral("exp(-t^2)",tlim=c(-2,3),ngrid=k,last=TRUE))
plot(n,i)

# Riemannian sums and integrals
fun=function(t) exp(-t^2)*2*t
integrate(fun,-2,3)

q=as.path("t^2",tlim=c(-2,3),ngrid=50)
p1=pIntegral("exp(-t^2)",driver=q,adjust="left");p1[length(p1)]
p2=pIntegral("exp(-t^2)",driver=q,adjust="right");p2[length(p2)]
p3=pIntegral("exp(-t^2)",driver=q,adjust="");p3[length(p3)]
p4=pIntegral("exp(-t^2)*2*t",tlim=c(-2,3),ngrid=50,adjust="left")
   p4[length(p4)]
p5=pIntegral("exp(-t^2)*2*t",tlim=c(-2,3),ngrid=50,adjust="right")
   p5[length(p5)]
p6=pIntegral("exp(-t^2)*2*t",tlim=c(-2,3),ngrid=50,adjust="")
   p6[length(p6)]
pPlot(pBind(p1,p2,p3,p4,p5,p6))

# Linear differential equation
p=as.path("3*t-t^2/2",tlim=c(0,5),ngrid=50)
q=pApply("exp(x)",p)
r=1+pIntegral("exp(x)",driver=p)
pPlot(pBind(q,r))

# Stochastic calculus
W=rpWiener(ngrid=5000)
p=W()
pPlot(p)
q1=pIntegral("x",p)
pPlot(q1)
q2=(p^2-pClock(p))/2
pPlot(q2)
pPlot(q1-q2,ylim=c(-1,1))

W=rpWiener(ngrid=5000)
p=W()
k=3
q1=p^k
pPlot(q1)
q2=k*pIntegral("x^(k-1)",p)+k*(k-1)/2*pIntegral("x^(k-2)",p,v=2)
pPlot(q2)
pPlot(q1-q2,ylim=c(-1,1))

# Discrete hedging
H=Hedge.Call.BS(strike=1,vol=0.2,rate=0.08,rlrate=0.03)
H(rebalance=10,plot=TRUE)
Hist(H(1000,rebalance=10))
}
}
