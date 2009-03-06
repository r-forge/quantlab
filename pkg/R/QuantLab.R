#chapter{QuantLab}

library(its)
library(abind)
library(Hmisc)

#section{Utilities}

Cat <- function(message){
    cat(paste(rep("\b",100),collapse=""),message)
    flush.console()
    }

is.scalar <- function(x){
   x=drop(x)
   if (is.vector(x)&(length(x)==1)) return(TRUE)
   else return(FALSE)
   }

Disp <- function(x){
   n=NROW(x)
   ii=floor(seq(from=1,to=n,length=11))
   return(round(x[ii,],4))
   }

mApply  <- function(x,fun,drop=TRUE){
       if (is.character(fun)) ff=function(x) eval(parse(text=fun))
       else ff=fun
       if (is.null(dim(x))) stop("missing dimension attribute")
       m=length(dim(x))
       if (m==1) test.in=1
       else {
           test.in=rep(1,prod(dim(x)[-m]))
           dim(test.in)=dim(x)[-m]
       }
       test.out=drop(ff(test.in))
       if (is.scalar(test.out)) dim.out=1
       else if (is.vector(test.out)) dim.out=length(test.out)
       else dim.out=dim(test.out)
       y=apply(x,m,ff)
       dim(y)=c(dim.out,dim(x)[m])
       if (drop) y=drop(y)
       return(y)
   }

#section{Plots}

Persp <- function(x,y=NULL,z=NULL,xlim=c(-3,3),ylim=c(-3,3),ngrid=30,zlim=NULL,
         phi=10,theta=30,pal=terrain.colors,control=TRUE,...){
   if (is.character(x)) {
      expr=x
      ff=function(x,y) eval(parse(text=expr))
      x=seq(from=xlim[1],to=xlim[2],length=ngrid)
      y=seq(from=ylim[1],to=ylim[2],length=ngrid)
      z=outer(x,y,ff)
      }
   m=length(x)
   n=length(y)
   cz=(z[1:(m-1),1:(n-1)]+z[1:(m-1),2:n]+z[2:m,1:(n-1)]+z[2:m,2:n])/4
   if (max(cz[])==min(cz[])) {
      c=1
      if (is.null(zlim)) zlim=c(min(cz[])-1,max(cz[])+1)
      }
   else {
      c=cut(cz[],20,labels=FALSE)
      if (is.null(zlim)) zlim=c(min(cz[]),max(cz[]))
      }
   persp(x,y,z,zlim=zlim,phi=phi,theta=theta,ltheta=45,ticktype="detailed",
         col=pal(20)[c],...)
   keybd=function(key) {
      if (key=="Up") phi <<- phi+1
      else if (key=="Down") phi <<- phi-1
      else if (key=="Right") theta <<- theta+1
      else if (key=="Left") theta <<- theta-1
      persp(x,y,z,zlim=zlim,phi=phi,theta=theta,ltheta=45,ticktype="detailed",
      col=pal(20)[c],...)
      if (key=="End") "Done" else NULL
      }
   if (control) getGraphicsEvent(onKeybd=keybd)
   }

Steps            <- function(x,t=NULL) UseMethod("Steps",x)
Steps.default    <- function(x,t=NULL){
     if (is.null(t)) t=1:length(x)
     plot(stepfun(t[-1],x),lty=3,
            verticals=TRUE,do.points=FALSE,ann=FALSE)
     lines(stepfun(t[-1],x),verticals=FALSE,do.points=FALSE,lty=1)
     }

pPlot <- function(x,mode="parallel",...){
   if (class(x)=="its") {
       plot(x,ylab="")
       title(main=paste(paste(names(x),collapse=", "),": ",start(x),"to",end(x)))
       }
   else if (mode=="parallel") {
       if (class(x)=="path") matplot(attr(x,"time"),x,type="l",ann=FALSE,...)
       else matplot(1:NROW(x),x,type="l",ann=FALSE,...)
       }
   else {
       stopifnot(length(dim(x))>1)
       plot(x[,1],x[,2],type="l",ann=FALSE,...)
       }
   }

#section{Empirical data}

GetStocks <- function(tickers="MSFT",from=c(1,1,2003),to=c(31,1,2003)){
   yahoo=function(ticker,from,to){
      str1="http://table.finance.yahoo.com/table.csv?"
      str2="&y=0&g=d&ignore=.csv"
      str=paste(str1,"a=",from[2]-1,"&b=",from[1],"&c=",from[3],"&d=",
         to[2]-1,"&e=",to[1],"&f=",to[3],"&s=",ticker,str2,sep="")
      return(str)
      }
   get.stock=function(ticker,from,to){
      x=read.csv(yahoo(ticker,from,to))
      n=dim(x)[1]
      x=x[n:1,]
      x$Date=as.POSIXct(strptime(as.character(x$Date),"%Y-%m-%d"))
      return(its(x[[7]],dates=x$Date,names=ticker))
      }
   y=numeric(0)
   for (i in 1:length(tickers)){
      x=get.stock(tickers[i],from,to)
      if (i==1) dates0=attr(x,"dates")
      y=cbind(y,x[,1])
      }
   return(its(y,dates=dates0,names=tickers))
   }

Returns <- function(prices){
   n=dim(prices)[1]
   z=as.matrix(prices)
   class(z)="matrix"
   y1=cbind(log(z[2:n,]/z[1:(n-1),]))
   return(its(y1,dates=attr(prices,"dates")[2:n],names=names(prices)))
   }


#section{Statistics}

Barplot          <- function(x,levels=sort(unique(x)),reference=NULL,...){
   if (is.null(reference)) barplot(table(factor(x,levels)),col=c("red"),...)
   else {
      y=table(factor(x,levels))/length(x)
      z=cbind(y,reference)
      barplot(t(z),beside=TRUE,col=c("red","green"),...)
      }
   }
Boxplot          <- function(x,...){
   boxplot(x,horizontal=TRUE,range=0,...)
   return()
   }
Hist             <- function(x,pdens=NULL,...){
   hist(x,probability=TRUE,main=NULL,xlab=NULL,ylab=NULL,plot=TRUE,ann=FALSE,...)
   if (!(is.null(pdens))) {
      y=seq(from=min(x),to=max(x),length=100)
      lines(y,pdens(y),type="l")
      }
   }
Ecdf             <- function(x,pcdf=NULL,...){
   plot(ecdf(x),do.points=FALSE,...)
   if (!(is.null(pcdf))) {
      y=seq(from=min(x),to=max(x),length=100)
      lines(y,pcdf(y),type="l")
      }
   }
Scatter <- function(x){
   x=drop(x)
   if (is.null(dim(x))) plot(sort(unique(x)),table(x),type="h",ann=FALSE)
   else if (dim(x)[2]==2) plot(x[,1],x[,2],type="p",ann=FALSE)
   else plot(x[1,],x[2,],type="p",ann=FALSE)
   }
Normalplot <- function(x){
   if (is.function(x)) x=sort(x(10000))
   else x=sort(unclass(x))
   temp=qqnorm(x)
   plot(temp$y,temp$x,type="l",xlab="data",ylab="normal scores")
   }

Shape <- function(x) {
   temp=summary(x)
   cat("Mean=",mean(x),"StDev=",sd(x),"\n")
   cat("Five point summary:",temp[c(1,2,3,5,6)],"\n")
   cat("Quartile skewness=",(temp[5]+temp[2]-2*temp[3])/(temp[5]-temp[2]),
   "Quartile kurtosis=",(temp[5]-temp[2])/sd(x),"\n")
   }

#section{Random}

MC <- function(rgen,iter=100000,blocks=10000,silent=FALSE){
   counter=0
   s=0
   sq=0
   n=iter
   while (iter>0) {
      bl=min(blocks,iter)
      temp=rgen(bl)
      if (is.null(dim(temp))|length(dim(temp))==1) {
         s=s+sum(temp)
         sq=sq+sum(temp^2)
         }
      else {
         m=length(dim(temp))
         s=s+rowSums(temp,m-1)
         sq=sq+rowSums(temp^2,m-1)
         }
      counter=counter+bl
      if (!silent) Cat(paste("iter = ",counter))
      iter=iter-bl
      }
   if (!silent) cat("\n")
   mn=s/n
   sd=sqrt(sq/n-mn^2)/sqrt(n)
   return(list(mean=mn,sdev=sd))
   }

rpRep            <- function(len=10,rgen=rNormal()){
    len=floor(len)
    z=rgen(1)
    if (is.array(z)) k=dim(z) else k=length(z)
    ff=function(n=1,drop=TRUE){
        x=rgen(len*n)
        dim(x)=c(k,len,n)
        i=length(dim(x))
        y=drop(aperm(x,c(i-1,1:(i-2),i)))
        if (!drop) dim(y)=c(len,k,n)
        return(y)
    }
    return(ff)
}

rBernoulli       <- function(p=0.5,values=c(1,0),rep=NULL){
   ff= function(n=1,drop=TRUE){
      x=as.numeric(runif(n)<p)
      y=values[2]+x*(values[1]-values[2])
      if (!drop) dim(y)=c(1,n)
      return(y)
      }
   if (!is.null(rep)) return(rpRep(rep,ff))
   return(ff)
# X=rBernoulli(); X(10)
# X=rBernoulli(rep=5); X(3)
   }
rSym             <- function(k=1,rep=NULL){
   a=diag(rep(1,k))
   a=rbind(a,-a)
   X=rSimple(1:(2*k))
   ff=function(n,drop=TRUE){
      y=t(a[X(n),])
      y=drop(y)
      if (!drop) dim(y)=c(k,n)
      return(y)
      }
   if (!is.null(rep)) return(rpRep(rep,ff))
   return(ff)
# X=rSym(); X(20)
# X=rSym(rep=5); X(10)
# X=rSym(k=2); X(10)
# X=rSym(k=2,rep=5); X(1); X(10)
   }
rSimple          <- function(table,rep=NULL){
   if (NCOL(table)==1)
      table=cbind(table,rep(1/length(table),length(table)))
   p=cumsum(table[,2]/sum(table[,2]))
   x=table[,1]
   ff=function(n=1,drop=TRUE){
      y=x[apply(outer(runif(n),p,">"),1,sum)+1]
      if (!drop) dim(y)=c(1,n)
      return(y)
      }
   if (!is.null(rep)) return(rpRep(rep,ff))
   return(ff)
# X=rSimple(1:6); X(10)
# X=rSimple(cbind(1:10,1/(1:10))); X(10)
# table(X(1000))
# X=rSimple(cbind(1:3,c(0.5,0.2,0.3)),rep=10); X(5)
}

rNormal          <- function(k=1,mean=0,cov=1,rep=NULL){
   if ((k==2)&(length(cov)==3))
   cov=matrix(c(cov[1]^2,cov[3]*cov[1]*cov[2],cov[3]*cov[1]*cov[2],cov[2]^2),2,2)
   ff= function(n=1,t=1,drop=TRUE){
      x=rnorm(n*k)
      dim(x)=c(n,k)
      if (is.scalar(cov)) x=x*sqrt(cov)*sqrt(t)
      else if (is.matrix(cov)==TRUE) x=(x%*%chol(cov))*sqrt(t)
      x=t(x)+outer(mean*rep(1,k),t*rep(1,n))
      if (drop) x=drop(x)
      return(x)
      }
   if (!is.null(rep)) return(rpRep(rep,ff))
   return(ff)
}

rpoisson         <- function(n,lambda=1,rgen=NULL){
   # First we need a sufficient number of Poisson variates:
   x=rpois(n,lambda)
   # The cases where the Poisson variate is zero need a special
   # treatment. In order to simplify matters we replace 0s by 1s but
   # remember where the 0s were located:
   ii=which(x==0)
   x[ii]=1
   # We generate a sufficient number of independent
   # replications of $X$, arranging them as rows of an
   # array $z$:
   if (is.null(rgen)) rgen=function(n,drop=FALSE) matrix(1,1,n)
   z=cbind(t(rgen(sum(x),drop=FALSE)))
   # A dummy factor
   d=rep(1:n,x)
   # gives for each row of $z$ the corresponding replication
   # of final array $y$.
   # The next step is to calculate the row sums of $z$ which
   # constitute the replicates of $Y$. This is done for each
   # column of $y$ separately:
   y=numeric(0)
   for (i in 1:dim(z)[2]) y=cbind(y,tapply(z[,i],d,sum))
   # Finally, we restore the 0s:
   y[ii,]=0
   if (NCOL(y)==1) y=as.vector(y)
   return(t(y))
   }
rPoisson         <- function(lambda=1,rgen=NULL,rep=NULL){
   ff=function(n=1,t=1,drop=TRUE){
      x=rpoisson(n,lambda*t,rgen)
      k=NROW(x)
      x=drop(x)
      if (!drop) dim(x)=c(k,n)
      return(x)
      }
   if (!is.null(rep)) return(rpRep(rep,ff))
   return(ff)
# X=rPoisson(lambda=5); X(30)
# X=rPoisson(rep=10); X(5)
# X=rPoisson(lambda=5,rgen=rNormal(k=2)); X(5)
# X=rPoisson(rgen=rNormal(k=3),rep=5); X(3)
}
rGamma           <- function(shape=1,rate=1,rep=NULL){
   ff=function(n=1,t=1,drop=TRUE){
      x=rgamma(n,shape*t,rate)
      if (!drop) dim(x)=c(1,n)
      return(x)
      }
   if (!is.null(rep)) return(rpRep(rep,ff))
   return(ff)
   }

rBS <- function (vol = 1, rate = 0, spot = 1, maturity = 1, rep = NULL)
{
    rate = rate * maturity
    vol = vol * sqrt(maturity)
    ff = function(n, drop = TRUE) {
        x = spot * exp(rate - vol^2/2 + vol * rnorm(n))
        if (!drop)
            dim(x) = c(1, length(x))
        return(x)
    }
    if (!is.null(rep))
        return(rpRep(rep, ff))
    return(ff)
}



#section{Paths}

ngrid.default <- 500

as.path          <- function(fun=NULL,tlim=c(0,1),time=NULL,ngrid=NULL){
   if (is.null(ngrid)) ngrid=ngrid.default
   if (is.null(time)) t=seq(from=tlim[1],to=tlim[2],length=ngrid+1)
   else t=time
   if (is.null(fun)) p=t
   else if (is.character(fun)) p=eval(parse(text=fun))
   else if (is.function(fun)) p=fun(t)
   else if (is.numeric(fun)) {
      t=seq(from=tlim[1],to=tlim[2],length=ngrid+1)
      k=(NROW(fun)-1)/tlim[2]
      if (NCOL(fun)==1) p=fun[floor(k*t)+1]
      else p=fun[floor(k*t)+1,]
      }
   attr(p,"time")=t
   class(p)="path"
   return(p)
   }

its2path <- function(y){
   temp=unclass(dates(y))
   time=(temp-temp[1])/24/60/60/365
   p=cbind(unclass(y))
   class(p)="path"
   attr(p,"time")=time
   return(p)
   }

print.path <- function(x,...) print(unclass(x))

Steps.path       <- function(x,t=NULL){Steps.default(x,t=attr(x,"time"))}

pCompress <- function(p){
   ff=function(x) c(x[length(x)],max(x),min(x),mean(x))
   if (is.null(dim(p))) return(cbind(ff(p)))
   else if (length(dim(p))==1) return(cbind(ff(drop(p))))
   else {
      m=length(dim(p))
      return(apply(p,m,ff))
      }
   }

pBind            <- function(p,...){
    r=cbind(p,...)
    class(r)="path"
    attr(r,"time")=attr(p,"time")
    return(r)
    }
pClock            <- function(p){
    q=attr(p,"time")
    class(q)="path"
    attr(q,"time")=attr(p,"time")
    return(q)
    }
pReduce     <- function(p,q=NULL,time=NULL){
    t1=attr(p,"time")
    if (is.null(q)) t2=time
    else t2=attr(q,"time")
    if (is.null(t2)) stop("time attribute missing")
    i=findInterval(t2,t1)
    stopifnot(all(i>0))
    r=p[i]
    attr(r,"time")=t2
    class(r)="path"
    return(r)
    }

pDelta            <- function(p){
   d=apply(cbind(p),2,diff)
   p1=rbind(rep(0,NCOL(d)),d)
   class(p1)="path"
   attr(p1,"time")=attr(p,"time")
   return(p1)
   }
pVariation        <- function(p,order=2){
   ff=function(x) cumsum(abs(diff(x))^order)
   v=apply(cbind(p),2,ff)
   p1=rbind(rep(0,NCOL(v)),v)
   class(p1)="path"
   attr(p1,"time")=attr(p,"time")
   return(p1)
   }
pLag              <- function(p){
   ff=function(x) c(0,x[-NROW(x)])
   if (is.null(dim(p))) p1=ff(p)
   else p1=apply(p,2,ff)
   class(p1)="path"
   attr(p1,"time")=attr(p,"time")
   return(p1)
   }
pIntegral        <- function(fun="x",driver=NULL,y=NULL,tlim=c(0,1),
       ngrid=NULL,v=1,adjust="left",last=FALSE){
       if (is.null(driver)) driver=as.path(tlim=tlim,ngrid=ngrid)
       x=driver
       t=attr(x,"time")
       dim(x)=c(length(t),NCOL(x))
       if (is.function(fun)) p=fun(x,t)
       else if (is.character(fun)) p=eval(parse(text=fun))
       class(p)="path"
       attr(p,"time")=t
       if (adjust=="left") q=apply(pLag(p)*pDelta(x)^v,2,cumsum)
       else if (adjust=="right") q=apply(p*pDelta(x)^v,2,cumsum)
       else q=apply((p+pLag(p))*pDelta(x)^v/2,2,cumsum)
       class(q)="path"
       attr(q,"time")=t
       if (last) return(q[length(t)])
       else(return(q))
   }
pBracket         <- function(p,q=NULL){
       if (is.null(q)) return(pVariation(p))
       else stopifnot(attr(p,"time")==attr(q,"time"))
       r=cumsum(pDelta(p)*pDelta(q))
       class(r)="path"
       attr(r,"time")=attr(p,"time")
       return(r)
   }
pExp             <- function(p){
       q=exp(p-pBracket(p)/2)
       class(q)="path"
       attr(q,"time")=attr(p,"time")
       return(q)
   }

pApply  <- function(fun="x",p=NULL,tlim=c(0,1),ngrid=NULL){
       if (is.null(p)) p=as.path(tlim=tlim,ngrid=ngrid)
       if (is.character(fun)) gg=function(x,t) eval(parse(text=fun))
       else gg=fun
       t=attr(p,"time")
       q=gg(p,t)
       class(q)="path"
       attr(q,"time")=t
       return(q)
   }

#rpApply <- function(rgen,fun) {
#   ff=function(n,drop=TRUE) mApply(rgen(n,drop=FALSE),fun,drop=drop)
#   }

#section{Processes}

rpRandomwalk     <- function(len=100,rgen=rSym(),start=0){
    len=floor(len)
    gg=function(x) c(0,cumsum(x))
    k=length(rgen(1))
    ff=function(n=1,drop=TRUE){
        x=rgen(len*n)
        dim(x)=c(k,len,n)
        x=apply(x,c(1,3),gg)+start
        #x=aperm(x,c(2,1,3))
        p=drop(x)
        if (!drop) dim(p)=c(len+1,k,n)
        return(p)
    }
    return(ff)
}

rpLevy <- function(rdist=rNormal(),tlim=c(0,1),time=NULL,ngrid=NULL){
   k=length(rdist(1))
   gg=function(x) c(0,cumsum(x))
   if (is.null(ngrid)) ngrid=ngrid.default
   if (is.null(time)) time=seq(from=tlim[1],to=tlim[2],
          length=ngrid+1)
   if ((length(time)==1)&(time[1]==0))
      ff=function(n=1,drop=TRUE) {
         p=rep(0,k*n)
         dim(p)=c(1,k,n)
         if (drop) p=drop(p)
         attr(p,"time")=0
         class(p)="path"
         return(p)
         }
   else if (length(time)==1)
      ff=function(n=1,drop=TRUE){
         p=rdist(n,time)
         dim(p)=c(k,1,n)
         p=aperm(p,c(2,1,3))
         if (drop) p=drop(p)
         attr(p,"time")=time
         class(p)="path"
         return(p)
         }
   else {
      origin=TRUE
      if (time[1]>0) {
         time=c(0,time)
         origin=FALSE
         }
      d=diff(time)
      ff=function(n=1,drop=TRUE){
         x=rdist(length(d)*n,d)
         dim(x)=c(k,length(d),n)
         x=aperm(x,c(2,1,3))
         p=apply(x,c(2,3),gg)
         if (drop) p=drop(p)
         if (!origin) {
            time=time[-1]
            if (is.vector(p)) p=p[-1,drop=drop]
            else if (length(dim(p))==2) p=p[-1,,drop=drop]
            else p=p[-1,,,drop=drop]
            }
         class(p)="path"
         attr(p,"time")=time
         return(p)
         }
      }
   return(ff)
   }

rpWiener         <- function(k=1,mean=0,cov=1,tlim=c(0,1),time=NULL,ngrid=NULL){
   x=rpLevy(rdist=rNormal(k,mean,cov),tlim,time,ngrid)
   return(x)
   }

rpPoisson        <- function(lambda=1,rgen=NULL,tlim=c(0,1),time=NULL,ngrid=NULL){
   x=rpLevy(rdist=rPoisson(lambda,rgen),tlim,time,ngrid)
   return(x)
# X=rpPoisson(time=1,lambda=50,rgen=rCusp(alpha=0.5))
# Normalplot(X)
# X=rpPoisson(lambda=50,rgen=rCusp(alpha=0.5))
# plot(X(1))
}

rpGamma          <- function(shape=1,rate=1,tlim=c(0,1),time=NULL,ngrid=NULL){
   x=rpLevy(rdist=rGamma(shape,rate),tlim,time,ngrid)
   return(x)
   }

rpBS  <- function(vol=1,rate=0,spot=1,tlim=c(0,1),time=NULL,ngrid=NULL){
   X=rpWiener(k=1,mean=rate,cov=vol^2,tlim,time,ngrid=ngrid)
   ff=function(n=1,drop=TRUE){
      p=X(n,drop=drop)
      t=attr(p,"time")
      q=spot*exp(p-vol^2*t/2)
      class(q)="path"
      attr(q,"time")=t
      return(q)
      }
   return(ff)
   }
rpEquation       <- function(start=1,drift=0,diffusion="x",driver=rpWiener()){
   if (is.function(drift)) ff1=drift
   else if (is.character(drift)) ff1=function(x,t) eval(parse(text=drift))
   else ff1=function(x,t) return(drift)
   if (is.function(diffusion)) ff2=diffusion
   else if (is.character(diffusion)) ff2=function(x,t) eval(parse(text=diffusion))
   else ff2=function(x,t) return(diffusion)
   ff=function(n=1,last=FALSE){
      p=driver(n)
      t=attr(p,"time")
      dim(p)=c(length(t),n)
      x=matrix(0,length(t),n)
      x[1,]=rep(start,n)
      for (i in (2:length(t))){
           x[i,]=x[i-1,]+ff1(x[i-1,],t[i-1])*(t[i]-t[i-1])+
              ff2(x[i-1,],t[i-1])*(p[i,]-p[i-1,])
      }
      if (last) return(x[length(t),])
      else {
         class(x)="path"
         attr(x,"time")=t
         return(x)
         }
      }
   return(ff)
   }

#section{Finance}

Call <- function(strike=1){
   if (length(strike)==1) ff=function(x) pmax(x-strike,0)
   else ff=function(x) pmax(outer(x,-strike,"+"),0)
   return(ff)
   }

Put <- function(strike=1){
   if (length(strike)==1) ff=function(x) pmax(-x+strike,0)
   else ff=function(x) pmax(outer(-x,strike,"+"),0)
   return(ff)
   }

BinCall <- function(strike=1){
   if (length(strike)==1) ff=function(x) pmax(x>strike,0)
   else ff=function(x) pmax(outer(x,strike,">"),0)
   return(ff)
   }

BinPut <- function(strike=1){
   if (length(strike)==1) ff=function(x) pmax(x<=strike,0)
   else ff=function(x) pmax(outer(x,strike,"<="),0)
   return(ff)
   }

KnockOutCall <- function(strike=1,barrier=1.5) {
   ns=length(strike)
   nb=length(barrier)
   if ((ns==1)&(nb>1)) strike=rep(strike,nb)
   if ((ns>1)&(nb==1)) barrier=rep(barrier,ns)
   ff=function(p) {
      x=pCompress(p)
      if (ns==1)
         return(pmax(x[1,]-strike,0)*(x[2,]<barrier))
      else
         return(pmax(outer(x[1,],-strike,"+"),0)*outer(x[2,],barrier,"<"))
      }
   return(ff)
   }

KnockInCall <- function(strike=1,barrier=1.5) {
   ns=length(strike)
   nb=length(barrier)
   if ((ns==1)&(nb>1)) strike=rep(strike,nb)
   if ((ns>1)&(nb==1)) barrier=rep(barrier,ns)
   ff=function(p) {
      x=pCompress(p)
      if (ns==1)
         return(pmax(x[1,]-strike,0)*(x[2,]>barrier))
      else
         return(pmax(outer(x[1,],-strike,"+"),0)*outer(x[2,],barrier,">"))
      }
   return(ff)
   }

AverageStrikeCall <- function(a,b) {
   ff=function(p) {
      x=pCompress(p)
      if (length(a)==1)
         return(pmax(x[1,]-a-b*x[4,],0))
      else
         return(pmax(outer(x[1,]-b*x[4,],-a,"+"),0))
      }
   return(ff)
   }

AverageRateCall <- function(a,b) {
   ff=function(p) {
      x=pCompress(p)
      if (length(a)==1)
         return(pmax(a+b*x[4,],0))
      else
         return(pmax(outer(b*x[4,],a,"+"),0))
      }
   return(ff)
   }

FloatingStrikePut <- function(a=1) {
   ff=function(p) {
      x=pCompress(p)
      if (length(a)==1)
         return(pmax(pmax(a,x[2,])-x[1,],0))
      else
         return(pmax(outer(x[2,],a,pmax)-x[1,],0))
      }
   return(ff)
   }

Call.BS <- function(vol=1,strike=1,rate=0,spot=1,maturity=1){
    x=spot/strike
    vol=vol*sqrt(maturity)
    rate=rate*maturity

    y=(log(spot/strike)+rate)/vol
    value=spot*pnorm(y+vol/2)-strike*pnorm(y-vol/2)*exp(-rate)
    ii=which(((spot==strike)&(vol==0))|is.na(value))
    value[ii]=0

    delta=pnorm(y+vol/2)
    ii=which(((spot==strike)&(vol==0))|is.na(delta))
    delta[ii]=0.5

    gamma=dnorm(y+vol/2)
    gamma=gamma/spot/vol
    ii=which(((spot==strike)&(vol==0))|is.na(gamma))
    gamma[ii]=0

    return(list(Value=value,Delta=delta,Gamma=gamma))
    }

Put.BS <- function(vol=1,strike=1,rate=0,spot=1,maturity=1){
    value=Call.BS(vol,strike,rate,spot,maturity)$Value-spot+strike*exp(-rate*maturity)
    delta=Call.BS(vol,strike,rate,spot,maturity)$Delta-1
    gamma=Call.BS(vol,strike,rate,spot,maturity)$Gamma
    return(list(Value=value,Delta=delta,Gamma=gamma))
    }

call.price.diagram <- function(vol=1,strike=1,rate=0,maturity=1,disp="Value"){
   spot=seq(from=0.1,to=1.5,length=100)*strike
   keybd=function(key) {
      if (key=="Up") maturity <<- min(maturity+0.01,3)
      else if (key=="Down") maturity <<- max(maturity-0.01,0)
      else if (key=="Right") vol <<- min(vol+0.01,3)
      else if (key=="Left") vol <<- max(vol-0.01,0)
      else if (key=="PgUp") rate <<- min(rate+0.01,1)
      else if (key=="PgDn") rate <<- max(rate-0.01,0)
      ff=function(spot) Call.BS(vol,strike,rate,spot,maturity)
      if (disp=="Value") {
         plot(spot,ff(spot)$Value,type="l",ann=FALSE,col="red",lwd=2);grid()
         lines(spot,Call(1)(spot),col="blue",lwd=2)
         title(main=paste("Call price: T=",round(maturity,2),
            ", vol=",round(vol,2),", rate=",rate),xlab="spot price",ylab="call price")
         }
      else if (disp=="Delta") {
         plot(spot,ff(spot)$Delta,type="l",ann=FALSE,col="red",lwd=2);grid()
         title(main=paste("Call delta: T=",round(maturity,2),
         ", vol=",round(vol,2),", rate=",rate),xlab="spot price",ylab="call delta")
         }
      else if (disp=="Gamma") {
         plot(spot,ff(spot)$Gamma,type="l",ann=FALSE,col="red",lwd=2);grid()
         title(main=paste("Call gamma: T=",round(maturity,2),
         ", vol=",round(vol,2),", rate=",rate),xlab="spot price",ylab="call gamma")
         }
      if (key=="End") "Done" else NULL
      }
   keybd("")
   getGraphicsEvent(onKeybd=keybd)
   }

put.price.diagram <- function(vol=1,strike=1,rate=0,maturity=1,disp="Value"){
   spot=seq(from=0.1,to=1.5,length=100)*strike
   keybd=function(key) {
      if (key=="Up") maturity <<- min(maturity+0.01,3)
      else if (key=="Down") maturity <<- max(maturity-0.01,0)
      else if (key=="Right") vol <<- min(vol+0.01,3)
      else if (key=="Left") vol <<- max(vol-0.01,0)
      else if (key=="PgUp") rate <<- min(rate+0.01,1)
      else if (key=="PgDn") rate <<- max(rate-0.01,0)
      ff=function(spot) Put.BS(vol,strike,rate,spot,maturity)
      if (disp=="Value") {
         plot(spot,ff(spot)$Value,type="l",ann=FALSE,col="red",lwd=2);grid()
         lines(spot,Put(1)(spot),col="blue",lwd=2)
         title(main=paste("Put price: T=",round(maturity,2),
            ", vol=",round(vol,2),", rate=",rate),xlab="spot price",ylab="put price")
         }
      else if (disp=="Delta") {
         plot(spot,ff(spot)$Delta,type="l",ann=FALSE,col="red",lwd=2);grid()
         title(main=paste("Put delta: T=",round(maturity,2),
         ", vol=",round(vol,2),", rate=",rate),xlab="spot price",ylab="put delta")
         }
      else if (disp=="Gamma") {
         plot(spot,ff(spot)$Gamma,type="l",ann=FALSE,col="red",lwd=2);grid()
         title(main=paste("Put gamma: T=",round(maturity,2),
         ", vol=",round(vol,2),", rate=",rate),xlab="spot price",ylab="put gamma")
         }
      if (key=="End") "Done" else NULL
      }
   keybd("")
   getGraphicsEvent(onKeybd=keybd)
   }

BinomTree <- function(n,up,down=1/up,rate,spot=1,claim=Call(1),mode="european"){
   s=list(spot)
   u=up^sqrt(1/n)
   d=down^sqrt(1/n)
   r=rate/n
   for (i in 1:n) s=c(s,list(d^(i:0)*u^(0:i)))
   v=s
   v[[n+1]]=claim(s[[n+1]])
   for (i in n:1) {
      hs=diff(v[[i+1]])/diff(s[[i+1]])
      hb=exp(-i*r)*(v[[i+1]][-1]-hs*s[[i+1]][-1])
      if (mode=="european") v[[i]]=hb*exp((i-1)*r)+hs*s[[i]]
      else v[[i]]=pmax(claim(s[[i]]),hb*exp((i-1)*r)+hs*s[[i]])
      }
   print(v[[1]])
   invisible(list(Stock=s,Value=v))
   }

CRR <- function(n,vol=1,rate=0,spot=1,maturity=1,claim=Call(1),mode="european"){
   v=BinomTree(n,up=exp(vol*sqrt(maturity)),rate=rate*maturity,
      spot=spot,claim=claim,mode=mode)
   invisible(v)
   }


Hedge.Call.BS <- function(strike=1,vol=1,rate=0,rlrate=0,start=1,horizon=1){
   ff=function(n=1,rebalance=100,plot=FALSE){
      if (plot==TRUE) n=1
      xx=rep(0,n)
      X=rpBS(vol,rate,start,
        tlim=c(0,horizon),time=NULL,ngrid=horizon*rebalance+1)
      for (jj in 1:n) {
         S=X(1)
         time=attr(S,"time")
         S=as.vector(S)
         c=Call.BS(vol,strike,rlrate,spot=S,maturity=horizon-time)
         Hedge=c$Delta*S
         nb=length(S)

         y=-Hedge[-1]+Hedge[-nb]/S[-nb]*S[-1]
         Cash=filter(y,exp(rlrate/rebalance),method="recursive",init=c$Value[1]-Hedge[1])
         Cash=c(c$Value[1]-Hedge[1],as.vector(Cash))
         Error=Hedge+Cash-c$Value
         xx[jj]=Error[rebalance+1]
         }
      if (n==1) {
         res=data.frame(Time=time,Stock=S,Delta=c$Delta,Gamma=c$Gamma,Value=c$Value,
            Hedge=Hedge,Cash=Cash,Portfolio=Hedge+Cash,Error=Error)
         attr(res,"time")=res$Time
         print(Disp(res))
         if (plot) {
            op=par(no.readonly = TRUE)
            layout(matrix(c(1,2),2,1),heights=c(2,1))
            par(mar=c(3,3,3,2))
            matplot(res$Time,cbind(res$Value,res$Portfolio),type="l",
               xlab="time",ylab="value",lty=c(1,1),col=c("black","red"),ann=FALSE)
            str=paste("Hedge: rebalance=",rebalance,", vol=",vol,
                ", rate=",rate,", rlrate=",rlrate,", strike=",strike)
            mtext(str,side=3,line=1)
            par(mar=c(3,3,1,2))
            plot(res$Time,res$Gamma,type="l",ann=FALSE)
            par(op)
            }
         invisible(res)
         }
      else {
         Shape(xx)
         invisible(xx)
         }
      }
   return(ff)
   }



#section{Rd-files}
