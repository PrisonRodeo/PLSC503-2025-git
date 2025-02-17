#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things                                ####
#
# PLSC 503 -- Spring 2025: Code for Week Five
#
# "Variance things"
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# Packages:

P<-c("RCurl","readr","plyr","car","psych","rms","plm",
     "lmtest","stargazer","MASS","dplyr","glmnet",
     "sandwich","stargazer")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# setwd() too...
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes")
#
# Options...

options(scipen = 9) # bias against scientific notation
options(digits = 3) # show fewer decimal places
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Handy "robust" summary function for lm...   ####
#
# (Not used here, but maybe useful.)
# 
# url_robust<-"https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
# eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
#      envir=.GlobalEnv)
# rm(url_robust)
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 2016 ANES Data                              ####
#
# ANES 2016 pilot study aggregation example...

ANES<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2025-git/master/Data/ANES-pilot-2016.csv")

ANES$ftgay<-ifelse(ANES$ftgay==998,NA,ANES$ftgay)

# Average feeling thermometers about gays and lesbians:

summary(ANES$ftgay)
summary(ANES$presjob)

# States:

ANES$State<-car::recode(ANES$state,
"1='AL';2='AK';4='AZ';5='AR';6='CA';8='CO';9='CT';
10='DE';11='DC';12='FL';13='GA';15='HI';16='ID';17='IL';
18='IN';19='IA';20='KS';21='KY';22='LA';23='ME';24='MD';
25='MA';26='MI';27='MN';28='MS';29='MO';30='MT';31='NE';
32='NV';33='NH';34='NJ';35='NM';36='NY';37='NC';38='ND';
39='OH';40='OK';41='OR';42='PA';44='RI';45='SC';46='SD';
47='TN';48='TX';49='UT';50='VT';51='VA';53='WA';54='WV';
55='WI';56='WY'")

# Aggregate by state:

ANES$one<-1
StateFT<-ddply(ANES,.(State),summarise,
               Nresp=sum(one),
               meantherm=mean(ftgay,na.rm=TRUE),
               meanpresapp=mean(presjob,na.rm=TRUE))
summary(StateFT)

respfit<-with(StateFT, lm(meantherm~log(Nresp)))

pdf("StateThermPlot24.pdf",6,5)
par(mar=c(4,4,2,2)) 
with(StateFT, plot(Nresp,meantherm,pch=".",col="white",log="x",
                   xlab="ln(N of Respondents)",xlim=c(0.5,200),
                   ylab="Statewide Mean Score"))
with(StateFT, text(Nresp,meantherm,log="x",labels=StateFT$State,
                   cex=0.3*log(StateFT$Nresp+1)))
abline(h=mean(ANES$ftgay,na.rm=TRUE),lwd=2)
abline(h=mean(StateFT$meantherm),lwd=2,lty=2,col="red")
abline(respfit,lwd=3,col="darkgreen")
legend("topright",bty="n",lwd=2,col=c("black","red","darkgreen"),
       lty=c(1,2,1),cex=0.7,legend=c("Individual-Level Mean",
                             "Mean of State Averages",
                             "Regression Line"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Weighted least squares...                   ####

pdf("WLS-Scatter.pdf",6,5)
par(mar=c(4,4,2,2))
with(StateFT, plot(meanpresapp,meantherm,pch=".",col="white",
                   xlab="Mean Obama (Dis)Approval",
                   ylab="Mean Gays+Lesbians FT",
                   xlim=c(2,6)))
with(StateFT, text(meanpresapp,meantherm,labels=StateFT$State,
                   cex=0.3*log(StateFT$Nresp+1)))
dev.off()

# Regressions:

ols<-lm(meantherm~meanpresapp,data=StateFT)
wls1<-lm(meantherm~meanpresapp,data=StateFT,weights=(log(StateFT$Nresp+1)))
wls2<-lm(meantherm~meanpresapp,data=StateFT,weights=(StateFT$Nresp))

stargazer(ols,wls1,wls2,
          column.labels=c("OLS","WLS [1/ln(N)]","WLS [1/N]"),
          dep.var.labels="Mean Gay/Lesbian FTs")

# Plot those:

pdf("WLS-Scatter2.pdf",6,5)
par(mar=c(4,4,2,2))
with(StateFT, plot(meanpresapp,meantherm,pch=".",col="white",
                   xlab="Mean Obama (Dis)Approval",
                   ylab="Mean Gays+Lesbians FT",
                   xlim=c(2,6)))
with(StateFT, text(meanpresapp,meantherm,labels=StateFT$State,
                   cex=0.3*log(StateFT$Nresp+1)))
abline(ols,lwd=2)
abline(wls1,lwd=2,lty=2,col="blue")
abline(wls2,lwd=2,lty=3,col="orange")
legend("bottomleft",bty="n",lty=c(1,2,3),lwd=2,
       col=c("black","blue","orange"),
       legend=c("OLS","WLS [weights=ln(N)]","WLS [weights=N]"))
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# What do "robust" SEs do? A simulation...    ####

set.seed(7222009)
X <- rnorm(10)
Y <- 1 + X + rnorm(10)
df10 <- data.frame(ID=seq(1:10),X=X,Y=Y)

fit10 <- lm(Y~X,data=df10)
summary(fit10)
rob10 <- vcovHC(fit10,type="HC1")
sqrt(diag(rob10))

# "Clone" each observation 100 times

df1K <- df10[rep(seq_len(nrow(df10)), each=100),]
df1K <- pdata.frame(df1K, index="ID")

fit1K <- lm(Y~X,data=df1K)
summary(fit1K)

# With clustered SEs (HC1):

clustSE<-sqrt(diag(vcovCL(fit1K,cluster=df1K$ID)))
clustOLS<-coeftest(fit1K,vcov.=vcovCL,cluster=~df1K$ID)
clustOLS

# Add a "robust" variance results to the stat-level
# aggregate regressions table...

rSE<-sqrt(diag(vcovHC(ols,type="const")))
rOLS<-coeftest(ols,vcov.=vcovHC)

stargazer(ols,rOLS,wls1,wls2,se=list(rSE),
          column.labels=c("OLS","OLS (robust)","WLS [1/ln(N)]","WLS [1/N]"),
          dep.var.labels="Mean Gay/Lesbian FTs")


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Make the 2016 NES state-level data for a regression:

ANES$RConserv<-ANES$lcself
ANES$RConserv<-ifelse(ANES$RConserv==8,NA,ANES$RConserv)
ANES$BornAgain<-ANES$pew_bornagain
ANES$BornAgain<-ifelse(ANES$BornAgain==8,NA,ANES$BornAgain)
ANES$BornAgain<- 1 - (ANES$BornAgain-1)
ANES$Age<-2016-ANES$birthyr
ANES$Education<-ANES$educ

StateData<-ddply(ANES,.(State),summarise,
               NResp=sum(one),
               LGBTTherm=mean(ftgay,na.rm=TRUE),
               MeanCons=mean(RConserv,na.rm=TRUE),
               MeanAge=mean(Age/10,na.rm=TRUE),
               MeanEducation=mean(Education,na.rm=TRUE),
               BornAgainProp=mean(BornAgain,na.rm=TRUE))
psych::describe(StateData)

# Fit a basic multivariate model:

OLS<-lm(LGBTTherm~MeanCons+MeanAge+MeanEducation+BornAgainProp,
        data=StateData)
summary(OLS)

# Robust SEs:

hccm(OLS,type="hc3") # "HC3" var-cov matrix
sqrt(diag(hccm(OLS,type="hc3"))) # "HC3" robust SEs
coeftest(OLS,vcov.=vcovHC)

# Comparing different "robust" estimators (a la Long
# & Ervin):

OLSBs<-coef(OLS)
Naive<-sqrt(diag(vcov(OLS)))
HC0<-sqrt(diag(hccm(OLS, type="hc0")))
HC1<-sqrt(diag(hccm(OLS, type="hc1")))
HC2<-sqrt(diag(hccm(OLS, type="hc2")))
HC3<-sqrt(diag(hccm(OLS, type="hc3")))

# Plot ("by-hand," sorta...):
pd<-data.frame(Beta=c("Intercept","Mean Conservatism","Mean Age / 10",
                            "Mean Education","Proportion Born-Again"),
                     OLSB=coef(OLS),
                     OLSSE=sqrt(diag(vcov(OLS))),
                     OLSHC0=sqrt(diag(hccm(OLS,type="hc0"))),
                     OLSHC1=sqrt(diag(hccm(OLS,type="hc1"))),
                     OLSHC2=sqrt(diag(hccm(OLS,type="hc2"))),
                     OLSHC3=sqrt(diag(hccm(OLS,type="hc3"))))
pd<-pd[2:5,]

pdf("RobustSEComparison-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(c(1:4)-0.1,pd$OLSB,pch=20,xaxt="n",
     xlim=c(0.5,4.5),ylim=c(-26,25),
     xlab="Variables",ylab="Estimates")
axis(1,at=c(1:4)+0.05,labels=pd$Beta,cex.axis=0.7)
segments(rep(1:4)-0.1,pd$OLSB-(1.96*pd$OLSSE),
         rep(1:4)-0.1,pd$OLSB+(1.96*pd$OLSSE))
points(rep(1:4),pd$OLSB,pch=15,col="red") # HC0
segments(rep(1:4),pd$OLSB-(1.96*pd$OLSHC0),
         rep(1:4),pd$OLSB+(1.96*pd$OLSHC0),col="red")
points(rep(1:4)+0.1,pd$OLSB,pch=18,col="blue") # HC1
segments(rep(1:4)+0.1,pd$OLSB-(1.96*pd$OLSHC1),
         rep(1:4)+0.1,pd$OLSB+(1.96*pd$OLSHC1),col="blue")
points(rep(1:4)+0.2,pd$OLSB,pch=4,col="darkgreen") # HC2
segments(rep(1:4)+0.2,pd$OLSB-(1.96*pd$OLSHC2),
         rep(1:4)+0.2,pd$OLSB+(1.96*pd$OLSHC2),col="darkgreen")
points(rep(1:4)+0.3,pd$OLSB,pch=25,col="orange") # HC2
segments(rep(1:4)+0.3,pd$OLSB-(1.96*pd$OLSHC3),
         rep(1:4)+0.3,pd$OLSB+(1.96*pd$OLSHC3),col="orange")
abline(h=0,lty=2)
legend("topleft",bty="n",pch=c(20,17,15,18,4,25),cex=0.9,
       legend=c("OLS","HC0","HC1","HC2","HC3"),
       col=c("black","red","blue","darkgreen","orange"))
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Collinearity, etc.                          ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# Let's go back to the state-level ANES data that we
# created up there ^...
#
# We'll create a percentage version of the "BornAgainProp"
# variable, then forget to remove the old one when we
# add the new one...
#
# (This gives us "Perfect multicollinearity"):

StateData$BornAgainPct<-StateData$BornAgainProp*100

NewOLS<-lm(LGBTTherm~MeanCons+MeanAge+MeanEducation+
        BornAgainProp+BornAgainPct,data=StateData)
summary(NewOLS)

# N = K

GulfStates<-StateData[StateData$State %in% c("FL","AL","MS","LA","TX"),]
OLS.Gulf<-lm(LGBTTherm~MeanCons+MeanAge+MeanEducation+
                BornAgainProp,data=GulfStates)
summary(OLS.Gulf)

# Multicollinearity examples:

with(StateData, cor(MeanCons,BornAgainProp))

ols1<-lm(LGBTTherm~MeanCons,data=StateData)
ols2<-lm(LGBTTherm~BornAgainProp,data=StateData)
ols3<-lm(LGBTTherm~MeanCons+BornAgainProp,data=StateData)

stargazer(ols1,ols2,ols3)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Impeachment example...                      ####

impeachment<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2025-git/master/Data/impeachment.csv")

summary(impeachment)

# Standardize all the variables:

ImpStd<-data.frame(scale(impeachment[,4:9]))
cor(ImpStd)

# OLS w/o intercept:

fit<-with(ImpStd,lm(votesum~pctbl96+unionpct+clint96+GOPmember+ADA98-1))
summary(fit)

vif(fit)

# Ridge regression:

X<-ImpStd[,2:6] # Predictors
Y<-ImpStd[,1]   # Response

ridge.fit<-glmnet(X,Y,alpha=0)

pdf("RidgeRegressions.pdf",10,6)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(ridge.fit,label=TRUE)
abline(h=0,lty=2)
matplot(log(ridge.fit$lambda), t(ridge.fit$beta),
        type="l",main="",lwd=2,
        xlab=expression(Log(lambda)),
        ylab=expression("Estimates of"~beta))
abline(h=0,lty=2)
legend("topright",bty="n",col=c("black","red","green3","blue","cyan"),
       lty=c(1,2,3,4,5),legend=c("Percent Black","Union Percent",
                                 "Clinton Percent","GOP","ADA Score"))
dev.off()

# Lasso:

lasso.fit<-glmnet(X,Y,alpha=1)

pdf("LassoRegressions.pdf",10,6)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(lasso.fit,label=TRUE)
abline(h=0,lty=2)
matplot(log(lasso.fit$lambda), t(lasso.fit$beta),
        type="l",main="",lwd=2,xlim=c(-6,0.5),
        xlab=expression(Log(lambda)),
        ylab=expression("Estimates of"~beta))
abline(h=0,lty=2)
legend("topright",bty="n",col=c("black","red","green3","blue","cyan"),
       lty=c(1,2,3,4,5),legend=c("Percent Black","Union Percent",
                                 "Clinton Percent","GOP","ADA Score"),
       cex=0.7)
dev.off()

# Cross-validation to get lambda...
#
# Ridge regression:

ridge.cv<-cv.glmnet(as.matrix(X),as.matrix(Y),alpha=0,intercept=FALSE)
ridge.cv
coef(ridge.cv,s="lambda.min")
coef(ridge.cv,s="lambda.1se")

# Lasso:

lasso.cv<-cv.glmnet(as.matrix(X),as.matrix(Y),alpha=1,intercept=FALSE)
lasso.cv
coef(lasso.cv,s="lambda.min")
coef(lasso.cv,s="lambda.1se")

# Plots:

pdf("GLMNET-CVPlots.pdf",10,6)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(ridge.cv)
legend("bottomright",bty="n",legend="Ridge Regression",cex=1.4)
plot(lasso.cv)
legend("topleft",bty="n",legend="Lasso",cex=1.4)
dev.off()

# /fin