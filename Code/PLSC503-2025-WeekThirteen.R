#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                   ####
#
# PLSC 503 -- Spring 2025
#
# Regression models for ordinal-level outcomes...
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.

P<-c("readr","MASS","nnet","VGAM","MNLpred","ordinal",
     "aod","car","ggplot2","scales","margins","psych",
     "dfidx","marginaleffects","modelsummary","tidyr",
     "vcd","stargazer")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# Run that ^ 10-12 times until it's all smileys :)
#
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd() as you like, or whatever, e.g.:
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes")
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Ordinal-response models...                          ####
# 
# GOP Thermometer score plot:

ANES<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2025-git/master/Data/ANES-pilot-2016.csv")

ANES$ftjeb<-ifelse(ANES$ftjeb==998,NA,ANES$ftjeb)

pdf("Notes/ANES-FT-Jeb-2016.pdf",7,6)
par(mar=c(4,4,2,2))
hist(ANES$ftjeb,breaks=seq(0,100,by=1),main="",
     xlab="Feeling Thermometer Score for Jeb!")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Ordered simulation:

set.seed(7222009)
X<-runif(1000,0,10)
Ystar<-0 + 1*X + rnorm(1000)
Y1<-Ystar
Y1[Ystar<2.5]<-1
Y1[Ystar>=2.5 & Ystar<5]<-2
Y1[Ystar>=5 & Ystar<7.5]<-3
Y1[Ystar>=7.5]<-4
table(Y1)

summary(lm(Ystar~X))
summary(lm(Y1~X))

pdf("OrdinalOneR.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(X,Ystar,pch=20,xlab="X",ylab="Y*")
abline(lm(Ystar~X),lwd=3,col="red")
abline(h=c(2.5,5,7.5),lty=2)
plot(X,Y1,pch=20,xlab="X",ylab="Y1")
abline(lm(Y1~X),lwd=3,col="red")
dev.off()

Y2<-Ystar
Y2[Ystar<2]<-1
Y2[Ystar>=2 & Ystar<8]<-2
Y2[Ystar>=8 & Ystar<9]<-3
Y2[Ystar>9]<-4
table(Y2)

summary(lm(Y2~X))

pdf("OrdinalTwoR.pdf",7,5)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(X,Ystar,pch=20,xlab="X",ylab="Y*")
abline(lm(Ystar~X),lwd=3,col="red")
abline(h=c(2,8,9),lty=2)
plot(X,Y2,pch=20,xlab="X",ylab="Y2")
abline(lm(Y2~X),lwd=3,col="red")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SCOTUS approval example (from TAPS - January 2018)      ####

SCdf<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2025-git/master/Data/SCOTUSApproval2018.csv")
SCdf<-as.data.frame(SCdf) # get rid of the damned tibble

describe(SCdf,skew=FALSE)

# Model using -polr-

SC.logit<-polr(as.factor(SCOTUSApproval)~KnowChiefJustice+Democrat+
                 GOP+Female+White+Black+Education+Age,data=SCdf)
summary(SC.logit)

# Same, using -clm- (in the -ordinal- package):

SC.logit2<-clm(as.factor(SCOTUSApproval)~KnowChiefJustice+Democrat+
                 GOP+Female+White+Black+Education+Age,data=SCdf)
summary(SC.logit2)

# Now, probit:

SC.probit<-polr(as.factor(SCOTUSApproval)~KnowChiefJustice+Democrat+
                  GOP+Female+White+Black+Education+Age,data=SCdf,
                  method="probit")
summary(SC.probit)

# Compare:

options("modelsummary_format_numeric_latex" = "plain")
modelsummary(list("Logit"=SC.logit,"Probit"=SC.probit),
             output="latex",stars=c("*"=0.05))

# Odds Ratios

olreg.or <- function(model) { 
  coeffs <- coef(summary(SC.logit)) 
  lci <- exp(coeffs[ ,1] - 1.96 * coeffs[ ,2]) 
  or <- exp(coeffs[ ,1]) 
  uci <- exp(coeffs[ ,1] + 1.96 * coeffs[ ,2]) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

olreg.or(SC.logit)

# Or using -modelsummary-:

modelsummary(list("Odds Ratios"=SC.logit),output="latex",
             stars=c("*"=0.05),exponentiate=TRUE)


# Predicted probabilities...

Sim<-data.frame(KnowChiefJustice=median(SCdf$KnowChiefJustice),
                Democrat=median(SCdf$Democrat),
                GOP=median(SCdf$GOP),
                Female=median(SCdf$Female),
                White=median(SCdf$White),
                Black=median(SCdf$Black),
                Education=mean(SCdf$Education),
                Age=seq(18,100,1))

SC.hat<-predict(SC.logit,Sim,type='probs')

pdf("SCOrdinalProbs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(c(min(Sim$Age),max(Sim$Age)),c(0,0.7),type='n',
     xlab="Age", ylab="Predicted Probability")
lines(min(Sim$Age):max(Sim$Age),SC.hat[,1],lty=1,lwd=3)
lines(min(Sim$Age):max(Sim$Age),SC.hat[,2],lty=2,lwd=3)
lines(min(Sim$Age):max(Sim$Age),SC.hat[,3],lty=3,lwd=3)
lines(min(Sim$Age):max(Sim$Age),SC.hat[,4],lty=4,lwd=3)
legend("topleft",bty="n",lwd=3,lty=c(1:4),
       legend=c("Pr(Strongly Disapprove)",
                "Pr(Somewhat Disapprove)",
                "Pr(Somewhat Approve)",
                "Pr(Strongly Approve)"))
dev.off()

# We can also do that using -plot_predictions- in the 
# -marginaleffects- package:

pdf("SCOrdinalProbs2.pdf",5,4)
p<-plot_predictions(SC.logit,condition=c("Age","group"),
                 type="probs",vcov=TRUE) +
  theme_classic() + ylab("Predicted Probabilities")
p
dev.off()

# ... and then use -ggplot2- commands to make it prettier...

# Note what happens if we do something that has a stronger
# relationship:

SCdf$Trump<-factor(SCdf$TrumpApproval,labels=c("Strongly Disapprove",
                   "Somewhat Disapprove","Somewhat Approve",
                   "Strongly Approve"))
DT.logit<-polr(Trump~Democrat+GOP+Female
               +White+Black+Education+Age,data=SCdf)
summary(DT.logit)

pdf("DTOrdinalProbs.pdf",7,4)
p<-plot_predictions(DT.logit,condition=c("Education","group"),
                    type="probs",vcov=TRUE) +
  theme_classic() + ylab("Predicted Probabilities")
p
dev.off()


# /fin