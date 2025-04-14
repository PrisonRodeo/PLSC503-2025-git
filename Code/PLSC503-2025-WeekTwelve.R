#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                   ####
#
# PLSC 503 -- Spring 2025
#
# Regression models for nominal-level outcomes...
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.

P<-c("readr","MASS","mlogit","nnet","VGAM","MNLpred",
     "aod","car","ggplot2","scales","margins","psych",
     "dfidx","marginaleffects","modelsummary","tidyr",
     "vcd","stargazer")

for(j in 1:length(P)) {
  for (i in 1:length(P)) {
    ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
           print(":)"))
    library(P[i],character.only=TRUE)
  }
j<-j+1
rm(i)
}
rm(j,P)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd() as you like, or whatever, e.g.:
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes")
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Multinomial logit...                           ####

NES92<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2025-git/master/Data/ANES92.csv")

describe(NES92)

# Three different ways to fit the same model...
#
# #1: Using -vglm-:

NES92.mlogit<-vglm(PresVote~PartyID+Age+White+Female,multinomial,data=NES92)
summary(NES92.mlogit)

# #2: using multinom (change the "baseline" category):

NES92$PresVote2<-factor(NES92$PresVote, 
                      levels = c("3", "1", "2"), 
                      labels = c("Perot", "Bush", "Clinton"))
NES92.mlogit2<-multinom(PresVote2~PartyID+Age+White+Female,data=NES92)
summary(NES92.mlogit2)

# #3: using -mlogit- (requires "reshaping" data):

head(NES92)
AltNES92<-dfidx(NES92,varying=9:11,shape="wide",choice="VotedFor")
head(AltNES92)

NES92.mlogit3<-mlogit(VotedFor~0|PartyID+Age+White+Female,
                      data=AltNES92,reflevel="Perot")
summary(NES92.mlogit3)

#=-=-=-=-=-=-=-=-=-=-=-=
# Different baselines:

Bush.nes92.mlogit<-vglm(PresVote~PartyID+Age+White+Female,
                        data=NES92,family=multinomial(refLevel=1)) 
summary(Bush.nes92.mlogit)

Clinton.nes92.mlogit<-vglm(PresVote~PartyID+Age+White+Female,
                        data=NES92,family=multinomial(refLevel=2)) 
summary(Clinton.nes92.mlogit)

# MNL vs. binary logit

NES92$PickBush<-NA
NES92$PickBush<-ifelse(NES92$VotedFor=="Bush",1,NES92$PickBush)
NES92$PickBush<-ifelse(NES92$VotedFor=="Perot",0,NES92$PickBush)
BushBinary<-glm(PickBush~PartyID+Age+White+Female,data=NES92,family="binomial")
summary(BushBinary)

NES92$PickClinton<-NA
NES92$PickClinton<-ifelse(NES92$VotedFor=="Clinton",1,NES92$PickClinton)
NES92$PickClinton<-ifelse(NES92$VotedFor=="Perot",0,NES92$PickClinton)
ClintonBinary<-glm(PickClinton~PartyID+Age+White+Female,data=NES92,family="binomial")
summary(ClintonBinary)

# Plot:

MNLBs<-as.numeric(t(coef(NES92.mlogit2))) # MNL betas
BinBs<-c(coef(BushBinary),coef(ClintonBinary)) # Binary Bs

pdf("MNLvsBinary.pdf",6,5)
par(mar=c(4,4,2,2))
plot(MNLBs,BinBs,pch=19,xlab="MNL Coefficient Estimates",
     ylab="Binary Logit Coefficient Estimates")
abline(a=0,b=1,lwd=2,col="red")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Conditional logit...        ####
#
# Data structure, revisited:

head(AltNES92)

# Conditional logistic regression (Feeling Thermomenter
# variable only):

NES92.clogit<-mlogit(VotedFor~FT,data=AltNES92,reflevel="Perot")
summary(NES92.clogit)

# "Full" model w/all predictors:

NES92.clogit2<-mlogit(VotedFor~FT|PartyID+Age+White+Female,
                     data=AltNES92,reflevel="Perot")
summary(NES92.clogit2)


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Interpretation!             ####

NES.MNL<-vglm(PresVote~PartyID+Age+White+Female,data=NES92,
              multinomial(refLevel=1)) # Bush is comparison category
summaryvglm(NES.MNL)

wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(5,6))
wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(1,3,5,7,9))

# Marginal effects, via -margins-...
#
# Recreate the results from above, using -multinom-:

MNL.alt<-multinom(PresVote2~PartyID+Age+White+Female,data=NES92,
                  Hess=TRUE)
summary(marginal_effects(MNL.alt))

# Odds ratios:

mnl.or <- function(model) { 
  coeffs <- c(t(coef(NES.MNL))) 
  lci <- exp(coeffs - 1.96 * diag(vcov(NES.MNL))^0.5) 
  or <- exp(coeffs) 
  uci <- exp(coeffs + 1.96* diag(vcov(NES.MNL))^0.5) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

mnl.or(NES.MNL)

# In-Sample predicted outcomes / PRE:

NES92$Predictions<-" "
NES92$Predictions<-ifelse(fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,2] 
                 & fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,3],
                 paste("Bush"),NES92$Predictions) # Bush
NES92$Predictions<-ifelse(fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,1] 
                 & fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,3],
                 paste("Clinton"),NES92$Predictions) # Clinton
NES92$Predictions<-ifelse(fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,1] 
                 & fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,2],
                 paste("Perot"),NES92$Predictions) # Perot)

table(NES92$VotedFor,NES92$Predictions)

# In-sample predictions:

hats<-as.data.frame(fitted.values(NES.MNL))
names(hats)<-c("Bush","Clinton","Perot")
attach(hats)

pdf("InSampleRScatterplotMatrix.pdf",8,7)
spm(~Bush+Clinton+Perot,pch=20,plot.points=TRUE,
    diagonal="histogram",col=c("black","grey"))
dev.off()

pdf("InSampleMNLPredProbsR.pdf",6,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(NES92$PartyID,Bush,xlab="Party ID",pch=20)
plot(NES92$PartyID,Clinton,xlab="Party ID",pch=20)
plot(NES92$PartyID,Perot,xlab="Party ID",pch=20)
par(mfrow=c(1,1))
dev.off()

# Predicted probabilities using -MNLpred-...
#
# Re-fit the -multinom- estimates again, changing 
# "White" to numeric:

NES92$WhiteNum<-ifelse(NES92$White=="White",1,0)
MNL.alt2<-multinom(PresVote2~PartyID+Age+WhiteNum+Female,
                   data=NES92,Hess=TRUE)
# summary(MNL.alt2)

# Predictions:

Hats<-mnl_pred_ova(model=MNL.alt2,data=NES92,
                   x="PartyID",by=0.1,seed=7222009,nsim=500)

# Plotting predicted probabilities & CIs
# (via ggplot; can also be done easily with 
# base R):

cand.labs <- c("Bush", "Clinton", "Perot")
names(cand.labs) <- c("1", "2", "3")

pdf("MNLPredictedProbabilities.pdf",7,5)
ggplot(data=Hats$plotdata,aes(x=PartyID,y=mean,
            ymin=lower,ymax=upper)) +
  geom_ribbon(alpha = 0.1) +
  geom_line() + theme_bw() +
  facet_wrap(PresVote2~.,scales="fixed",
             labeller=labeller(PresVote2=cand.labs)) +
  scale_x_continuous(breaks=1:7) +
  labs(y = "Predicted Probabilities",x = "Party Identification")
dev.off()

# Plotting first differences for the WHITE variable:

FDF<-mnl_fd2_ova(model=MNL.alt2,data=NES92,x="WhiteNum",
                 value1=min(NES92$WhiteNum),
                 value2=max(NES92$WhiteNum),nsim=500)

pdf("MNLFirstDifferences.pdf",7,5)
ggplot(FDF$plotdata_fd,aes(categories, y=mean,
                           ymin=lower,max=upper)) +
  geom_pointrange() + geom_hline(yintercept=0) +
  scale_y_continuous(name="First Difference: White") +
  labs(x = "Candidate") + theme_bw()
dev.off()

# Conditional logit: In-sample predictions:

summary(NES92.clogit2)

CLhats<-predict(NES92.clogit2,AltNES92)

# Plot by candidate:

pdf("InSampleCLHatsR.pdf",7,6)
par(mar=c(4,4,2,2))
plot(NES92$FT.Bush,CLhats[,2],pch=19,
     col=rgb(100,0,0,100,maxColorValue=255),
     xlab="Feeling Thermometer",
     ylab="Predicted Probability")
points(NES92$FT.Clinton+runif(nrow(CLhats),-1,1),
       CLhats[,3],pch=4,col=rgb(0,0,100,100,maxColorValue=255))
points(NES92$FT.Perot+runif(nrow(CLhats),-1,1),
       CLhats[,1],pch=17,col=rgb(0,100,0,50,maxColorValue=255))
lines(lowess(NES92$FT.Bush,CLhats[,2]),lwd=2,col="red")
lines(lowess(NES92$FT.Clinton,CLhats[,3]),lwd=2,col="blue")
lines(lowess(NES92$FT.Perot,CLhats[,1]),lwd=2,col="darkgreen")
legend("topleft",bty="n",c("Bush","Clinton","Perot"),
       col=c("red","blue","darkgreen"),pch=c(19,4,17))
dev.off()


# /fin