#Script: Econometrics Paper: Steam Games Data
#Author: William Elijah Clark
#Date: 11/28/2016

#Import the data
library(readr)
Steam_Data_Collection_For_R_Project_v2 <- read_csv("All Documents/Academic Non-Art/UCF Undergrad/Econometrics/Project/Steam_Data_Collection_For_R_Project_v2.csv")

#Objects for full price and discounted price
pfull <- (Steam_Data_Collection_For_R_Project_v2$pfull)
pdisc <- (Steam_Data_Collection_For_R_Project_v2$pdisc)

#Objects for (Proxy) Quantity of Games in Libraries before and after sale
ownbe <- (Steam_Data_Collection_For_R_Project_v2$ownbe)
ownaf <- (Steam_Data_Collection_For_R_Project_v2$ownaf)

#Logarithmic Transformation of ownbe and ownaf
ownbe1 <-log(ownbe, base = exp(1))
ownaf1 <-log(ownaf, base = exp(1))

#Month and Genre Objects
month <- (Steam_Data_Collection_For_R_Project_v2$month)
genre <- (Steam_Data_Collection_For_R_Project_v2$genre)

#Change in price and quantities
deltp <- pfull-pdisc
deltq <- ownbe-ownaf

#Elasticity function
elast <- deltq/deltp

#Object transformation based on further findings
ownbe1alt <- 1/ownbe1

#Means of data
mean(elast, na.rm=TRUE)

mean(ownaf1)

#Genre Classifications: Acad = Action-Adventure, Shoot = Shooter, Strat = Strategy, Rpg = RPG, Misc = Miscellaneous
#Levels for Genre
levels(genre)

#Means for prices for each game genre type
mean(pfull[genre=="Acad"])
mean(pdisc[genre=="Acad"], na.rm = TRUE)
mean(pfull[genre=="Shoot"])
mean(pdisc[genre=="Shoot"], na.rm = TRUE)
mean(pfull[genre=="Strat"])
mean(pdisc[genre=="Strat"], na.rm = TRUE)
mean(pfull[genre=="Rpg"])
mean(pdisc[genre=="Rpg"], na.rm = TRUE)
mean(pfull[genre=="Misc"])
mean(pdisc[genre=="Misc"], na.rm = TRUE)

#Mean game quantities owned before and after sales
mean(ownbe[genre=="Acad"])
mean(ownaf[genre=="Acad"])
mean(ownbe[genre=="Shoot"])
mean(ownaf[genre=="Shoot"])
mean(ownbe[genre=="Strat"])
mean(ownaf[genre=="Strat"])
mean(ownbe[genre=="Rpg"])
mean(ownaf[genre=="Rpg"])
mean(ownbe[genre=="Misc"])
mean(ownaf[genre=="Misc"])


#####################################
#Some Demand Curve Regression Models#
#####################################

#Notes on object naming:
#demandbX = demand before discounting
#demandaX = demand after discounting

demandb1 <- lm(pfull~ownbe1)
demanda1 <- lm(pdisc~ownaf1, na.action=na.exclude)
demandb2 <- lm(pfull~ownbe1+genre)
demanda2 <- lm(pdisc~ownaf1+genre, na.action=na.exclude)
demandb3 <- lm(pfull~ownbe1+genre+month)
demanda3 <- lm(pdisc~ownaf1+genre+month, na.action=na.exclude)
demandb4 <- lm(ownbe1~pfull+genre)
demanda4 <- lm(ownaf1~pdisc+genre, na.action=na.exclude)
demandb5 <- lm(ownbe1~pfull+genre+month)
demanda5 <- lm(ownaf1~pdisc+genre+month, na.action=na.exclude)

#Some transformed demand curves
demandb3alt1 <- lm(pfull~sqrt(ownbe1)+genre+month)
demandb3alt2 <- lm(pfull~1/(ownbe1)+genre+month)
demandb3alt3 <- lm(pfull~ownbe1alt+genre+month)

#Demand curves with before AND after variables included
demandd <- lm(pfull~ownbe1+ownaf1+pdisc+genre+month)
demande <- lm(ownaf1~pfull+pdisc+ownbe1+genre+month)


###############################################
#All Demand Regression Summaries for Inference#
###############################################

#Summaries for Regression *before* discounted price
summary(demandb1)
summary(demandb2)
summary(demandb3)
summary(demandb4)
summary(demandb5)
summary(demandb3alt1)
summary(demandb3alt2)
summary(demandb3alt3)

#Summaries for Regression *after* discounted price
summary(demanda1)
summary(demanda2)
summary(demanda3)
summary(demanda4)
summary(demanda5)

#Summaries with both before and after variables
summary(demandd)
summary(demande)

################################################
#Simple Regression Plots and Graphs with Base R#
################################################

plot(pdisc ~ ownaf1, xlab = "quantity", ylab = "price")
abline(demanda1)
title(main="demand curve a1", col.main="black", font.main=1)

plot(pfull ~ ownbe1, xlab = "quantity", ylab = "price")
abline(demandb1)
title(main="demand curve b1", col.main="black", font.main=1)


########################
#Regression Diagnostics#
########################

library(car)

#Diagnostics for before or after only
crPlots(demandb1)
title(main="demand curve b1 Diagnostic", col.main="black", font.main=1)
crPlots(demanda1)
title(main="demand curve a1 Diagnostic", col.main="black", font.main=1)
crPlots(demandb2)
title(main="demand curve b2 Diagnostic", col.main="black", font.main=1)
crPlots(demanda2)
title(main="demand curve a2 Diagnostic", col.main="black", font.main=1)
crPlots(demandb3)
title(main="demand curve b3 Diagnostic", col.main="black", font.main=1)
crPlots(demandb4)
title(main="demand curve b4 Diagnostic", col.main="black", font.main=1)
crPlots(demanda4)
title(main="demand curve a4 Diagnostic", col.main="black", font.main=1)
crPlots(demandb5)
title(main="demand curve b5 Diagnostic", col.main="black", font.main=1)
crPlots(demanda5)
title(main="demand curve a5 Diagnostic", col.main="black", font.main=1)

#Diagnostics for transformed regressions
crPlots(demandb3alt1)
title(main="demand curve b3alt1 Diagnostic", col.main="black", font.main=1)
crPlots(demandb3alt2)
title(main="demand curve b3alt2 Diagnostic", col.main="black", font.main=1)
crPlots(demandb3alt3)
title(main="demand curve b3alt2 Diagnostic", col.main="black", font.main=1)

#Diagnostics for regressions with before AND after
crPlots(demandd)
title(main="demand curve d Diagnostic", col.main="black", font.main=1)
crPlots(demande)
title(main="demand curve e Diagnostic", col.main="black", font.main=1)


#GGPlot2 QQ Plot Graph#
install.packages("ggplot2")
demande.stdres = rstandard(demande)
qqnorm(demande.stdres,  ylab="standardized residuals",  xlab="normal scores",  main="NormalQQ plot for demande")


#VIF testing for multicollinearity
vif(demande)
sqrt(vif(demande))

#DurbinWatson Autocorrelation testing
durbinWatsonTest(demande)


#Residual testing
library(MASS)
qqPlot(demande, main="QQ Plot")
sresit <-studres(demande)
hist(sresid, freq=FALSE,
     +      main="Distribution of Studentized Residuals")
hist(sresit, freq=FALSE,
       +      main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
xfit<-seq(min(sresit),max(sresit),length=40) 
yfit<-dnorm(xfit)
lines(xfit, yfit) 

############
#Some plots#
############

#Plots for univariate demand regression

plot(ownbe1 ~ pdisc, xlab = "quantity", ylab = "price")
plot(ownbe1 ~ pdisc, xlab = "price", ylab = "quantity")
abline(derp)
title(main="Univariate Demand Regression", col.main="black", font.main=1)
crPlots(derp)
plot(ownbe1 ~ pdisc, xlab = "quantity", ylab = "price")
plot(ownbe1 ~ pdisc, xlab = "price", ylab = "quantity")
abline(derp)
title(main="figure1", col.main="black", font.main=1)


#Histograms for good measure
hist(pfull)
hist(ownbe)
hist(ownbe1)


