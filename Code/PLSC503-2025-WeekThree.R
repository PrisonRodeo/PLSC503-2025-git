#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro materials                               ####
#
# PLSC 503 -- Spring 2025
#
# Code for Week Three ("Practical Regression")
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# setwd() here...
#
# setwd(~/Whatever)
#
# Packages:

P<-c("readr","car","psych","stargazer","lmtest",
     "plotrix","dplyr","dotwhisker")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# Also, some preferences:

options(scipen=9)
options(digits=2)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Regression things...
#
# "Irrelevant" Z correlated with X but not with Y...
#
# Simulate *one* regression:

N<-50

set.seed(7222009)
X<-rnorm(N)         # Predictor
Z<-(X+rnorm(N))/1.5 # Z "caused by" X
Y<-X+rnorm(N)       # Outcome Y (unrelated to Z)

print(summary(lm(Y~X)))
print(summary(lm(Y~X+Z)))

# Do this 1000 times:

R<-1000
BX<-numeric(R)
BZ<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  X<-rnorm(N)
  Z<-(X+rnorm(N))/1.5
  Y<-X+rnorm(N)
  f<-lm(Y~X+Z)
  BX[i]<-coef(f)[2]
  BZ[i]<-coef(f)[3]
}

# Plot:

pdf("IrrPredictor.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX),xlim=c(-1.2,2.2),lwd=2,
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
abline(v=1,lty=2,col="grey")
abline(v=0,lty=2,col="orange")
legend("topright",lwd=2,lty=c(1,2),col=c("black","orange","white"),
       bty="n",legend=c("Predictor","Irrelevant Z"," (N=50)"))
dev.off()

# Make N 10x larger...

N<-500
R<-1000
BX<-numeric(R)
BZ<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  X<-rnorm(N)
  Z<-(X+rnorm(N))/1.5
  Y<-X+rnorm(N)
  f<-lm(Y~X+Z)
  BX[i]<-coef(f)[2]
  BZ[i]<-coef(f)[3]
}

# Plot:

pdf("IrrPredictor2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX),xlim=c(-1.2,2.2),lwd=2,
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
abline(v=1,lty=2,col="grey")
abline(v=0,lty=2,col="orange")
legend("topright",lwd=2,lty=c(1,2),col=c("black","orange","white"),
       bty="n",legend=c("Predictor","Irrelevant Z"," (N=500)"))
dev.off()

# Next, a Z that is correlated with Y but not with X...

N<-50

set.seed(7222009)
X<-rnorm(N)         # Predictor
Z<-rnorm(N)         # Z orthogonal to X
Y<-X+Z+rnorm(N)     # Outcome Y 

print(summary(lm(Y~X)))
print(summary(lm(Y~X+Z)))

# Now some sims:

N<-50
R<-1000
BX<-numeric(R)
BZ<-numeric(R)
BX0<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  X<-rnorm(N)
  Z<-rnorm(N)
  Y<-X+Z+rnorm(N)
  f1<-lm(Y~X)
  f2<-lm(Y~X+Z)
  BX0[i]<-coef(f1)[2]  
  BX[i]<-coef(f2)[2]
  BZ[i]<-coef(f2)[3]
}

# Plot:

pdf("UncorrPredictors.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX),xlim=c(0,2),lwd=2,ylim=c(0,3),
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
lines(density(BX0),lwd=2,lty=3,col="navy")
abline(v=1,lty=2,col="grey")
legend("topright",lwd=2,lty=c(1,2,3),bty="n",
       col=c("black","orange","navy","white"),
       legend=c("Predictor","Z","Predictor w/o Z","   (N=50)"))
dev.off()

# Increase N to 500, same...

N<-500
R<-1000
BX<-numeric(R)
BZ<-numeric(R)
BX0<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  X<-rnorm(N)
  Z<-rnorm(N)
  Y<-X+Z+rnorm(N)
  f1<-lm(Y~X)
  f2<-lm(Y~X+Z)
  BX0[i]<-coef(f1)[2]  
  BX[i]<-coef(f2)[2]
  BZ[i]<-coef(f2)[3]
}

# Plot:

pdf("UncorrPredictors2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX),xlim=c(0,2),lwd=2,ylim=c(0,10),
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
lines(density(BX0),lwd=2,lty=3,col="navy")
abline(v=1,lty=2,col="grey")
legend("topright",lwd=2,lty=c(1,2,3),bty="n",
       col=c("black","orange","navy","white"),
       legend=c("Predictor","Z","Predictor w/o Z","   (N=500)"))
dev.off()

# Next: Z is a confounder...

N<-50
R<-1000
BX<-numeric(R)
BZ<-numeric(R)
BX0<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  Z<-rnorm(N)
  X<-(Z+rnorm(N))/1.5
  Y<-X+Z+rnorm(N)
  f1<-lm(Y~X)
  f2<-lm(Y~X+Z)
  BX0[i]<-coef(f1)[2]  
  BX[i]<-coef(f2)[2]
  BZ[i]<-coef(f2)[3]
}

# Plot:

pdf("Confounder.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX),xlim=c(0,2.5),lwd=2,ylim=c(0,2.5),
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
lines(density(BX0),lwd=2,lty=3,col="navy")
abline(v=1,lty=2,col="grey")
legend("topleft",lwd=2,lty=c(1,2,3),bty="n",
       col=c("black","orange","navy","white"),
       legend=c("Predictor","Confounder Z","Predictor w/o Z","  (N=50)"))
dev.off()

# Same with N=500:

N<-500
R<-1000
BX<-numeric(R)
BZ<-numeric(R)
BX0<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  Z<-rnorm(N)
  X<-(Z+rnorm(N))/1.5
  Y<-X+Z+rnorm(N)
  f1<-lm(Y~X)
  f2<-lm(Y~X+Z)
  BX0[i]<-coef(f1)[2]  
  BX[i]<-coef(f2)[2]
  BZ[i]<-coef(f2)[3]
}

# Plot:

pdf("Confounder2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX),xlim=c(0,2.5),lwd=2,ylim=c(0,7),
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
lines(density(BX0),lwd=2,lty=3,col="navy")
abline(v=1,lty=2,col="grey")
legend("topleft",lwd=2,lty=c(1,2,3),bty="n",
       col=c("black","orange","navy","white"),
       legend=c("Predictor","Confounder Z","Predictor w/o Z","  (N=500)"))
dev.off()

# Now, a collider:

N<-50
R<-1000
BX<-numeric(R)
BZ<-numeric(R)
BX0<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  X<-rnorm(N)
  Y<-X+rnorm(N)
  Z<-(X+Y+rnorm(N))/1.5
  f1<-lm(Y~X)
  f2<-lm(Y~X+Z)
  BX0[i]<-coef(f1)[2]  
  BX[i]<-coef(f2)[2]
  BZ[i]<-coef(f2)[3]
}

# Plot:

pdf("Collider.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX0),xlim=c(-1,2),lwd=2,ylim=c(0,4),
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
lines(density(BX),lwd=2,lty=3,col="navy")
abline(v=1,lty=2,col="grey")
legend("topleft",lwd=2,lty=c(1,2,3),bty="n",
       col=c("black","orange","navy","white"),
       legend=c("Predictor","Collider Z","Predictor w/Z","  (N=50)"))
dev.off()

# Same w / N=500:

N<-500
R<-1000
BX<-numeric(R)
BZ<-numeric(R)
BX0<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  X<-rnorm(N)
  Y<-X+rnorm(N)
  Z<-(X+Y+rnorm(N))/1.5
  f1<-lm(Y~X)
  f2<-lm(Y~X+Z)
  BX0[i]<-coef(f1)[2]  
  BX[i]<-coef(f2)[2]
  BZ[i]<-coef(f2)[3]
}

# Plot:

pdf("Collider2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX0),xlim=c(-1,2),lwd=2,ylim=c(0,11),
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
lines(density(BX),lwd=2,lty=3,col="navy")
abline(v=1,lty=2,col="grey")
legend("topleft",lwd=2,lty=c(1,2,3),bty="n",
       col=c("black","orange","navy","white"),
       legend=c("Predictor","Collider Z","Predictor w/Z","  (N=500)"))
dev.off()

# Last: Mediators!

N<-50
R<-1000
BX<-numeric(R)
BZ<-numeric(R)
BX0<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  X<-rnorm(N)
  Z<-X+rnorm(N) / 1.6 # Z influenced by X
  Y<-(X+Z+rnorm(N))   # Y = f(X,Z)
  f1<-lm(Y~X)         # No Z ("total" X effect)
  f2<-lm(Y~X+Z)       # With Z ("direct" X effect)
  BX0[i]<-coef(f1)[2]  
  BX[i]<-coef(f2)[2]
  BZ[i]<-coef(f2)[3]
}

# Plot:

pdf("Mediator.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX),xlim=c(-0.5,3),lwd=2,ylim=c(0,3),
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
lines(density(BX0),lwd=2,lty=3,col="navy")
abline(v=1,lty=2,col="grey")
legend("topleft",lwd=2,lty=c(1,2,3),bty="n",
       col=c("black","orange","navy","white"),
       legend=c("Predictor","Mediator Z","Predictor w/o Z","  (N=50)"))
dev.off()

# Now with N=500:

N<-500
R<-1000
BX<-numeric(R)
BZ<-numeric(R)
BX0<-numeric(R)

set.seed(7222009)
for(i in 1:R){
  X<-rnorm(N)
  Z<-X+rnorm(N) / 1.6 # Z influenced by X
  Y<-(X+Z+rnorm(N))   # Y = f(X,Z)
  f1<-lm(Y~X)         # No Z ("total" X effect)
  f2<-lm(Y~X+Z)       # With Z ("direct" X effect)
  BX0[i]<-coef(f1)[2]  
  BX[i]<-coef(f2)[2]
  BZ[i]<-coef(f2)[3]
}

# Plot:

pdf("Mediator2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(BX),xlim=c(-0.5,3),lwd=2,ylim=c(0,8.5),
     xlab="Estimates of Beta",main="")
lines(density(BZ),lwd=2,lty=2,col="orange")
lines(density(BX0),lwd=2,lty=3,col="navy")
abline(v=1,lty=2,col="grey")
legend("topleft",lwd=2,lty=c(1,2,3),bty="n",
       col=c("black","orange","navy","white"),
       legend=c("Predictor","Mediator Z","Predictor w/o Z","  (N=500)"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
