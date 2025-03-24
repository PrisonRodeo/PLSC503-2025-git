#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                     ####
#
# PLSC 503 - Spring 2025
#
# Maximum likelihood (MLE), Day One code...
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load packages:

P<-c("maxLik","distr")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# Set a few options...

options(scipen=999)
options(digits=4)
par(mar=c(4,4,2,2))

# Probably want to setwd() in here somewhere, too, or
# whatever...

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# MLE: Toy example                                 ####

data<-c(64,63,59,71,68)       # Data
Lis<-dnorm(data,mean=68,sd=4) # Likelihoods for m=68,s=4
Lis                           # List those likelihoods for each obs.
L68.4<-prod(Lis)              # The likelihood (joint product) for m=68,s=4

Mus<-seq(62,68,by=0.1)  # possible values of mu [62,68]
L<-numeric(length(Mus)) # a place to put the likelihoods

# Calculate likelihoods for different values of mu:

for (i in 1:length(Mus)) {
  L[i]<-prod(dnorm(data,mean=Mus[i],sd=4))
}

# Plot:

pdf("SalaryLR.pdf",5,4)
plot(Mus,L,t="l",lwd=2,xlab=expression(hat(mu)),
     ylab="Likelihood")
dev.off()

# Log-L Plot:

lnL<-numeric(length(Mus)) # a place to put the lnLs

for (i in 1:length(Mus)) {
  lnL[i]<-sum(log(dnorm(data,mean=Mus[i],sd=4)))
}

pdf("SalarylnLR.pdf",5,4)
plot(Mus,lnL,t="l",lwd=2,xlab=expression(hat(mu)),
     ylab="Log-Likelihood")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Taylor series plot:

x <- seq(-6,6,0.01)
fx <- sin(x)
a <- -1       # pick a value of x at which to locate the approximation...

# recall that the first derivative of sin(x) is cos(x) and 
# vice-versa. So...

TS1 <- sin(a) + (cos(a)/factorial(1))*(x-a) # 1st-order
TS2 <- sin(a) + (cos(a)/factorial(1))*(x-a) - 
  (sin(a)/factorial(2))*((x-a)^2) # 2nd-order
TS3 <- sin(a) + (cos(a)/factorial(1))*(x-a) + 
  (-sin(a)/factorial(2))*((x-a)^2) +
  (-cos(a)/factorial(3))*((x-a)^3) # 3rd-order

# Plot:

pdf("TaylorSeriesIllustrated.pdf",7,6)
par(mar=c(4,4,2,2))
plot(x,fx,t="l",lwd=4,ylim=c(-4,4),
     xlab="x",ylab="f(x) = sin(x)")
points(a,sin(a),pch=19,cex=1.5)
lines(x,TS1,lwd=3,lty=2,col="red")
lines(x,TS2,lwd=3,lty=3,col="blue")
lines(x,TS3,lwd=3,lty=4,col="gold")
#abline(h=0,lwd=1,lty=2)
abline(v=a,lwd=1,lty=2)
text(a-0.5,-4,paste0("a=",a))
legend(a+0.25,-2,bty="n",legend=c("f(x) = sin(x)",
                             "First-Order / Linear",
                             "Second-Order / Quadratic",
                             "Third-Order / Cubic"),
       lwd=c(4,3,3,3),lty=c(1,2,3,4),
       col=c("black","red","blue","gold"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# OPTIMIZATION bit...                              ####

set.seed(7222009)
U<-runif(100)
rayleigh<-3*sqrt(-2*log(1-U))
loglike <- function(param) {
  b <- param[1]
  ll <- (log(x)-log(b^2)) + ((-x^2)/(2*b^2))
  ll
}

# Fit...

x<-rayleigh
hatsNR <- maxLik(loglike, start=c(1))
summary(hatsNR)

# Comparing optimizers:

hatsBFGS <- maxLik(loglike, start=c(1),method="BFGS") # BFGS
hatsBHHH <- maxLik(loglike, start=c(1),method="BHHH") # BHHH
labels<-c("Newton-Raphson","BFGS","BHHH")

hats<-c(hatsNR$estimate,hatsBFGS$estimate,hatsBHHH$estimate) # Estimates
ses<-c(sqrt(-(1/(hatsNR$hessian))),sqrt(-(1/(hatsBFGS$hessian))),sqrt(-(1/(hatsBHHH$hessian)))) # SEs
its<-c(hatsNR$iterations,hatsBFGS$iterations,hatsBHHH$iterations) # Iterations

pdf("Rayleigh-Optims.pdf",10,6)
par(mfrow=c(1,3))
dotchart(hats,labels=labels,groups=NULL,pch=16,xlab="Estimate")
dotchart(ses,labels=labels,pch=16,xlab="Standard Error")
dotchart(its,labels=labels,pch=16,xlab="Iterations")
dev.off()

# Bad MLE: "Boundary" parameter...

set.seed(7222009)
bad<-runif(100)
bad<-0.000001*sqrt(-2*log(1-bad))
x<-bad
hatsBad <- maxLik(loglike, start=c(1))
summary(hatsBad)

# Bad MLE: Another "boundary" parameter...

set.seed(7222009)
alsobad<-runif(100)
alsobad<-999999999*sqrt(-2*log(1-alsobad))
x<-alsobad
hatsBad2 <- maxLik(loglike, start=c(1))
summary(hatsBad2)
