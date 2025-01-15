# Make the data for PLSC 503 Exercise One
# (Spring 2025 remix)... massive credit to
# Vincent Arel-Bundock (https://github.com/vincentarelbundock)
# for the -countrycode- and -WDI- R packages.
#
# Preliminaries:
#
# setwd("~/Dropbox (Personal)/PLSC 503/Exercises") # <-- modify as required
#
# Install these packages before loading them:

library(readr)
library(countrycode)
library(WDI)

# Get WDI data from 2019:

wdi<-WDI(country="all",start=2019,end=2019,
         indicator=c("Population"="SP.POP.TOTL", # Population (in, like, people)
                     "UrbanPopulation"="SP.URB.TOTL.IN.ZS", # Urban Population (% of total)
                     "BirthRatePer1K"="SP.DYN.CBRT.IN", # Birth Rate (births per 1K people)
                     "FertilityRate"="SP.DYN.TFRT.IN", # Fertility Rate (births per woman)
                     "PrimarySchoolAge"="SE.PRM.AGES", # Primary school starting age (years)
                     "TotalTrade"="NE.TRD.GNFS.ZS", # Total trade, % of GDP
                     "FDIIn"="BX.KLT.DINV.WD.GD.ZS", # FDI in, % of GDP
                     "PublicEdExpend"="SE.XPD.TOTL.GD.ZS", # Government expenditure on education (% of GDP)
                     "PublicHealthExpend"="SH.XPD.GHED.GD.ZS", # Public expenditure on health (% of GDP)
                     "WomenBusLawIndex"="SG.LAW.INDX", # Women Business & the Law Index Score
                     "MilitaryExpenditures"="MS.MIL.XPND.GD.ZS", # Military expenditures, % of GDP
                     "CO2Emissions"="EN.GHG.CO2.PC.CE.AR5")) # CO2 Emissions (metric tons per capita)

# Remove aggregates (e.g., "World," "Arab World," etc.):

wdi$ISO3<-countrycode(wdi$iso2c,origin="iso2c",destination="iso3c")
wdi<-wdi[is.na(wdi$ISO3)==FALSE,]

# rename Year:

wdi$Year<-wdi$year
wdi$year<-NULL

# Delete ISO2:

wdi$iso2c<-NULL

# Create a "region" variable

wdi$Region<-countrycode(wdi$ISO3,origin="iso3c",destination="region")

# Put ISO3 + Year + Region at the front of the data:

nc<-ncol(wdi)
sb<-seq(nc-2,nc)
se<-seq(1,(nc-3))
wdi<-wdi[,c(sb,se)]
rm(nc,sb,se)

# Output the file:

write.csv(wdi,"PLSC503-2025-ExerciseOne.csv",row.names=FALSE)

# /fin