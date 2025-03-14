#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Prefatory material...                         ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# PLSC 503 week one slides (2025 remix).
#
# Bivariate regression review...
#
# Packages, etc. (there aren't many):

P<-c("readr","psych","car","stargazer",
     "ggplot2","marginaleffects")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# ^ run that a few times until you get all-smileys...
# :)
#
# Set significant digits:

options(digits=4)

# Be sure to setwd() to the right space...
#
# e.g.:
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes/")
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Running infant mortality example...
#
# Read in data (from repo):

Data<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2025-git/master/Data/IMDPT25.csv")

# Summary statistics

with(Data, describe(InfantMortalityPerK))
with(Data, describe(DPTpct))

# Regression:

IMDPT<-lm(InfantMortalityPerK~DPTpct,data=Data,na.action=na.exclude)
summary.lm(IMDPT)

# Scatterplot:

pdf("IMDPT25.pdf",7,7) # <- create PDF
with(Data, plot(DPTpct,InfantMortalityPerK,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Infant Mortality (Deaths per 1000 Births)"))
with(Data, text(DPTpct,InfantMortalityPerK,labels=CCode))
with(Data, abline(v=mean(DPTpct,na.rm=TRUE),lty=2))
with(Data, abline(h=mean(InfantMortalityPerK,na.rm=TRUE),lty=2))
abline(IMDPT,lwd=3)
dev.off()

# ANOVA

anova(IMDPT)

# Residuals (u):

Data$IMDPTres <- with(Data, residuals(IMDPT))
describe(Data$IMDPTres)

# Residual density plot:

pdf("IMDPTResidualsDensity25.pdf",6,6)
with(Data, plot(density(IMDPTres,na.rm=TRUE),
                main="Density Plot: Regression Residuals",
                xlab="Residual Value"))
abline(v=0,lty=2,lwd=2)
dev.off()

# Fitted Values:

Data$IMDPThat<-fitted.values(IMDPT)
describe(Data$IMDPThat)

# Densities plot:

pdf("IMDPTFittedDensity25.pdf",6,6)
with(Data, plot(density(IMDPThat,na.rm=TRUE),
                main="Density Plot: Actual and Fitted Values",
                xlab="Values of Y"))
with(Data, lines(density(InfantMortalityPerK,na.rm=TRUE),
                 lty=2,col="red"))
with(Data, abline(v=mean(InfantMortalityPerK,na.rm=TRUE),
                  lty=2,lwd=2))
dev.off()

# Correlations:

with(Data, cor(InfantMortalityPerK,DPTpct,use="complete.obs"))
with(Data, cor(IMDPTres,InfantMortalityPerK,use="complete.obs"))
with(Data, cor(IMDPTres,DPTpct,use="complete.obs"))
with(Data, cor(IMDPThat,InfantMortalityPerK,use="complete.obs"))
with(Data, cor(IMDPThat,DPTpct,use="complete.obs"))
with(Data, cor(IMDPTres,IMDPThat,use="complete.obs"))

# Plotting residuals vs. X:

pdf("IMDPTResiduals25.pdf",7,7)
with(Data, plot(DPTpct,IMDPTres,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Residuals (Y - Y-hat)"))
with(Data, text(DPTpct,IMDPTres,labels=CCode))
abline(h=0,lwd=2)
dev.off()

# Squared residuals vs. X:

pdf("IMDPTSquaredResiduals25.pdf",7,7)
with(Data, plot(DPTpct,IMDPTres^2,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Residuals (Y - Y-hat)"))
with(Data, text(DPTpct,IMDPTres^2,labels=CCode))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Inference...                                  ####

# Estimated variance-covariance matrix:

vcov(IMDPT)

# Confidence intervals around the betas:

confint(IMDPT)

# Change the significance level...

confint(IMDPT,level=0.99)

# Predictions w/standard errors:

SEs<-predict(IMDPT,interval="confidence")
SEs

# Plot it!:

Sort<-order(Data$DPTpct)

pdf("IMDPT-CI25.pdf",6,5)
plot(Data$DPTpct,Data$InfantMortalityPerK,
     xlab="DPT Immunization Percentage",
     ylab="Infant Mortality Per 1000 Births")
abline(IMDPT,lwd=3)
lines(sort(Data$DPTpct),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(Data$DPTpct),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Easier, using -marginaleffects-:

pdf("IMDPT-CI-again.pdf",6,5)
plot_predictions(IMDPT,condition="DPTpct",points=1) +
  theme_classic()
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Model fit and R-squared...                  ####
#
# # Simulations:
# 
# seed <- 7222009
# set.seed(seed)
# 
# X<-rnorm(250)
# Y1<-5+2*X+rnorm(250,mean=0,sd=sqrt(0.2))
# Y2<-5+2*X+rnorm(250,mean=0,sd=sqrt(20))
# fit<-lm(Y1~X)
# summary(fit)
# 
# pdf("TightLine-R.pdf",5,5)
# plot(X,Y1,pch=20,xlab="X",ylab="Y",
#      xlim=c(-2.5,2.5),ylim=c(-10,15))
# abline(fit,lwd=3)
# text(-1.5,12,labels="R-squared = 0.95")
# dev.off()
# 
# fit2<-lm(Y2~X)
# summary(fit2)
# 
# pdf("ScatteredLine-R.pdf",5,5)
# plot(X,Y2,pch=20,xlab="X",ylab="Y",
#      xlim=c(-2.5,2.5),ylim=c(-10,15))
# abline(fit2,lwd=3)
# text(-1.5,12,labels="R-squared = 0.20")
# dev.off()

# R^2 = 0 plots:

seed<-7222009
set.seed(seed)

pdf("RSqZero.pdf",7,6)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
X<-(runif(100))*10
Yzero<-(runif(100))*10
Yquad<-30-((X-5)^2)+(2*runif(100))
Ystep<-ifelse(abs(X-5)>2.5,5+runif(100),1+runif(100))
Ytype<-rep(0:1,50)
Yvar<-ifelse(Ytype==1,X+(2*runif(50)),10-X+2*runif(50))
plot(Yzero~X,xlab="X", ylab="Y",pch=20)
abline(lm(Yzero~X),lwd=3)
plot(Yquad~X,xlab="X", ylab="Y",pch=20)
abline(lm(Yquad~X),lwd=3)
plot(Ystep~X,xlab="X", ylab="Y",pch=20)
abline(lm(Ystep~X),lwd=3)
plot(Yvar[Ytype==0]~X[Ytype==0],xlab="X", ylab="Y",pch=20)
points(Yvar[Ytype==1]~X[Ytype==1],pch="o")
abline(lm(Yvar~X),lwd=3)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Reporting                               ####

summary.lm(IMDPT)

# Easy LaTeX table using *stargazer*:

stargazer(IMDPT,
          type="latex",
          title="OLS Regression Model of Infant Mortality Rates, 2000",
          dep.var.caption="",
          dep.var.labels="Model I",
          digits=2,
          no.space=TRUE,
          intercept.top=TRUE,intercept.bottom=FALSE,
          covariate.labels=c("(Constant)","
                             DPT Immunization Rate"))

# /fin