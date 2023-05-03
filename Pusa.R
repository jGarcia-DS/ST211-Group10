library(arm)
library(ggplot2)
library(car)
library(tidyr)
library(dplyr)
library(gridExtra)

options(max.print=9999999) # So we can increase the amount that is printed in R

dat.pusa <- read.csv("EOTST2112023_PUSA.csv", header = TRUE)
# Removing NSID
dat.pusa <- dat.pusa[, -1]
# Making sure that the outcome is fully observed
dat.pusa <- subset(dat.pusa, W8PUSA >= 0) 
dat.pusa$W8PUSA[dat.pusa$W8PUSA==0]<- 0.05

rownames(dat.pusa) <- 1:nrow(dat.pusa)
dat.pusa[dat.pusa <= -1] <- NA

summary(dat.pusa)

## Needed to be below 1000 to have a good look at all the boxplots
dat.pusa.norich<-subset(dat.pusa,W8PUSA<=5000)

# continuous data scatterplots
special.dat <- gather(data = dat.pusa.norich[, c(1, 2, 28, 39, 41, 42, 49, 50)], -W8PUSA, key = "var", value = "value")
p1 <- ggplot(special.dat, aes(x = value, y = W8PUSA)) + geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~var, scales = "free_x")
p1 <- p1 + theme_bw() + scale_fill_grey() + theme(legend.position = "none")
p1

# categorical data boxplots
special.dat <- gather(data = dat.pusa.norich[, -c(1, 2, 28, 39, 41, 42, 49)], -W8PUSA, key = "var", value = "value")
p2 <- ggplot(special.dat, aes(x = factor(value), y = W8PUSA)) + geom_point(size = 0.5) +
  geom_boxplot() + facet_wrap(~var, scales = "free_x")

p2 <- p2 + theme_bw() + scale_fill_grey() + theme(legend.position = "none")
p2

proportion <- sum(is.na(dat.pusa$W1GrssyrMP)) / length(dat.pusa$W1GrssyrMP)
proportion

proportion <- sum(is.na(dat.pusa$W1yschat1)) / length(dat.pusa$W1yschat1)
proportion

proportion <- sum(is.na(dat.pusa$W2ghq12scr)) / length(dat.pusa$W2ghq12scr)
proportion

proportion <- sum(is.na(dat.pusa$W6DebtattYP)) / length(dat.pusa$W6DebtattYP)
proportion

proportion <- sum(is.na(dat.pusa$W8DGHQSC)) / length(dat.pusa$W8DGHQSC)
proportion

proportion <- sum(is.na(dat.pusa$W8DAGEYCH)) / length(dat.pusa$W8DAGEYCH)
proportion

## Linear models of predictors that we removed
# W1GrssyrMP
summary(lm(W8PUSA ~ W1GrssyrMP    
           , data = dat.pusa))

# W8DAGEYCH
summary(lm(W8PUSA ~ W8DAGEYCH    
           , data = dat.pusa))


# Converting columns to categorical predictors using factor
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,
          32,33,34,35,36,37,38,40,43,44,45,46,47,48)
dat.pusa[cols] <- lapply(dat.pusa[cols], factor)

# Merging categorical predictors
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(1) = 'FullTimePaid'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(2) = 'PartTimePaid'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.pusa$W1wrk1aMP<-relevel(dat.pusa$W1wrk1aMP,ref="Other")

dat.pusa$W1condur5MP <- with(dat.pusa, Recode(W1condur5MP, "c(1) = 'Yes'"))
dat.pusa$W1condur5MP <- with(dat.pusa, Recode(W1condur5MP, "c(2) = 'No'"))
dat.pusa$W1condur5MP<-relevel(dat.pusa$W1condur5MP,ref="Yes")

dat.pusa$W1hea2MP <- with(dat.pusa, Recode(W1hea2MP, "c(1) = 'Yes'"))
dat.pusa$W1hea2MP <- with(dat.pusa, Recode(W1hea2MP, "c(2) = 'No'"))
dat.pusa$W1hea2MP<-relevel(dat.pusa$W1hea2MP,ref="No")

dat.pusa$W1NoldBroHS <- with(dat.pusa, Recode(W1NoldBroHS, "c(0) = 0"))
dat.pusa$W1NoldBroHS <- with(dat.pusa, Recode(W1NoldBroHS, "c(1) = 1"))
dat.pusa$W1NoldBroHS <- with(dat.pusa, Recode(W1NoldBroHS, "c(2) = 2"))
dat.pusa$W1NoldBroHS <- with(dat.pusa, Recode(W1NoldBroHS, "c(3,4,5,6,7,8,9) = '>3'"))
dat.pusa$W1NoldBroHS<-relevel(dat.pusa$W1NoldBroHS,ref='0')

# Based on sample size
dat.pusa$W1hous12HH <- with(dat.pusa, Recode(W1hous12HH, "c(1) = 'OwnedOutright'"))
dat.pusa$W1hous12HH <- with(dat.pusa, Recode(W1hous12HH, "c(2) = 'BeingBought'"))
dat.pusa$W1hous12HH <- with(dat.pusa, Recode(W1hous12HH, "c(3,4,5,6,7,8) = 'Other'"))
dat.pusa$W1hous12HH<-relevel(dat.pusa$W1hous12HH,ref='Other')

dat.pusa$W1hiqualmum <- with(dat.pusa, Recode(W1hiqualmum, "c(2) = 'FirstDegree'"))
dat.pusa$W1hiqualmum <- with(dat.pusa, Recode(W1hiqualmum, "c(7) = 'A-Levels'"))
dat.pusa$W1hiqualmum <- with(dat.pusa, Recode(W1hiqualmum, "c(15) = 'GCSEa-c'"))
dat.pusa$W1hiqualmum <- with(dat.pusa, Recode(W1hiqualmum, "c(16) = 'GCSEd-e'"))
dat.pusa$W1hiqualmum <- with(dat.pusa, Recode(W1hiqualmum, "c(20) = 'NoQualMentioned'"))
dat.pusa$W1hiqualmum <- with(dat.pusa, Recode(W1hiqualmum, "c(1,3,4,5,6,8,9,10,11,12,13,14,17,18,19) = 'Other'"))
dat.pusa$W1hiqualmum<-relevel(dat.pusa$W1hiqualmum,ref='NoQualMentioned')

dat.pusa$W1wrkfulldad <- with(dat.pusa, Recode(W1wrkfulldad, "c(1) = 'FullTime'"))
dat.pusa$W1wrkfulldad <- with(dat.pusa, Recode(W1wrkfulldad, "c(2) = 'PartTime'"))
dat.pusa$W1wrkfulldad <- with(dat.pusa, Recode(W1wrkfulldad, "c(3) = 'NotWorking'"))
dat.pusa$W1wrkfulldad<-relevel(dat.pusa$W1wrkfulldad,ref='FullTime')

dat.pusa$W1wrkfullmum <- with(dat.pusa, Recode(W1wrkfullmum, "c(1) = 'FullTime'"))
dat.pusa$W1wrkfullmum <- with(dat.pusa, Recode(W1wrkfullmum, "c(2) = 'PartTime'"))
dat.pusa$W1wrkfullmum <- with(dat.pusa, Recode(W1wrkfullmum, "c(3) = 'NotWorking'"))
dat.pusa$W1wrkfullmum<-relevel(dat.pusa$W1wrkfullmum,ref='FullTime')

dat.pusa$W1ch0_2HH <- with(dat.pusa, Recode(W1ch0_2HH, "c(0) = '0'"))
dat.pusa$W1ch0_2HH <- with(dat.pusa, Recode(W1ch0_2HH, "c(1) = '1'"))
dat.pusa$W1ch0_2HH <- with(dat.pusa, Recode(W1ch0_2HH, "c(2,3) = '>1'"))
dat.pusa$W1ch0_2HH<-relevel(dat.pusa$W1ch0_2HH,ref='0')

dat.pusa$W1ch3_11HH <- with(dat.pusa, Recode(W1ch3_11HH, "c(0) = '0'"))
dat.pusa$W1ch3_11HH <- with(dat.pusa, Recode(W1ch3_11HH, "c(1) = '1'"))
dat.pusa$W1ch3_11HH <- with(dat.pusa, Recode(W1ch3_11HH, "c(2,3,4,5) = '>1'"))
dat.pusa$W1ch3_11HH<-relevel(dat.pusa$W1ch3_11HH,ref='0')

dat.pusa$W1ch12_15HH <- with(dat.pusa, Recode(W1ch12_15HH, "c(0) = '0'"))
dat.pusa$W1ch12_15HH <- with(dat.pusa, Recode(W1ch12_15HH, "c(1) = '1'"))
dat.pusa$W1ch12_15HH <- with(dat.pusa, Recode(W1ch12_15HH, "c(2,3,4) = '>1'"))
dat.pusa$W1ch12_15HH<-relevel(dat.pusa$W1ch12_15HH,ref='0')

dat.pusa$W1ch16_17HH <- with(dat.pusa, Recode(W1ch16_17HH, "c(0) = '0'"))
dat.pusa$W1ch16_17HH <- with(dat.pusa, Recode(W1ch16_17HH, "c(1) = '1'"))
dat.pusa$W1ch16_17HH <- with(dat.pusa, Recode(W1ch16_17HH, "c(2,3,4) = '>1'"))
dat.pusa$W1ch16_17HH<-relevel(dat.pusa$W1ch16_17HH,ref='0')

# W1IndSchool not changing

dat.pusa$W1marstatmum <- with(dat.pusa, Recode(W1marstatmum, "c(2) = 'Married'"))
dat.pusa$W1marstatmum <- with(dat.pusa, Recode(W1marstatmum, "c(5) = 'Divorced'"))
dat.pusa$W1marstatmum <- with(dat.pusa, Recode(W1marstatmum, "c(1,3,4,6,7) = 'Other'"))
dat.pusa$W1marstatmum<-relevel(dat.pusa$W1marstatmum,ref='Other')

dat.pusa$W1nssecfam <- with(dat.pusa, Recode(W1nssecfam, "c(1) = 'HigherManagerial/Professional'"))
dat.pusa$W1nssecfam <- with(dat.pusa, Recode(W1nssecfam, "c(2) = 'LowerManagerial/Professional'"))
dat.pusa$W1nssecfam <- with(dat.pusa, Recode(W1nssecfam, "c(4) = 'SmallEmployers'"))
dat.pusa$W1nssecfam <- with(dat.pusa, Recode(W1nssecfam, "c(3,5,6,7,8) = 'Other'"))
dat.pusa$W1nssecfam<-relevel(dat.pusa$W1nssecfam,ref='HigherManagerial/Professional')

dat.pusa$W1ethgrpYP <- with(dat.pusa, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.pusa$W1ethgrpYP <- with(dat.pusa, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.pusa$W1ethgrpYP <- with(dat.pusa, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.pusa$W1ethgrpYP <- with(dat.pusa, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.pusa$W1ethgrpYP <- with(dat.pusa, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.pusa$W1ethgrpYP<-relevel(dat.pusa$W1ethgrpYP,ref="White")

dat.pusa$W1heposs9YP <- with(dat.pusa, Recode(W1heposs9YP, "c(1,2) = 'Likely'"))
dat.pusa$W1heposs9YP <- with(dat.pusa, Recode(W1heposs9YP, "c(3,4) = 'NotLikely'"))
dat.pusa$W1heposs9YP<-relevel(dat.pusa$W1heposs9YP,ref="Likely")

dat.pusa$W1hwndayYP <- with(dat.pusa, Recode(W1hwndayYP, "c(0,1) = '0-1'"))
dat.pusa$W1hwndayYP <- with(dat.pusa, Recode(W1hwndayYP, "c(2,3) = '2-3'"))
dat.pusa$W1hwndayYP <- with(dat.pusa, Recode(W1hwndayYP,"c(4,5) = '4-5'"))
dat.pusa$W1hwndayYP<-relevel(dat.pusa$W1hwndayYP,ref="0-1")
# Not dealing with W1truantYP

# same as W1alceverYP

# W1bulrc

# W1disabYP

# W2disc1YP

# W2depressYP

# W6JobYP

# W6UnivYP

# W6EducYP

# W6Apprent1YP

dat.pusa$W6acqno <- with(dat.pusa, Recode(W6acqno, "c(1) = 'DegreeHE'"))
dat.pusa$W6acqno <- with(dat.pusa, Recode(W6acqno, "c(3,4) = 'A-levels'"))
dat.pusa$W6acqno <- with(dat.pusa, Recode(W6acqno,"c(5,6) = 'GCSE'"))
dat.pusa$W6acqno <- with(dat.pusa, Recode(W6acqno,"c(7,8) = 'Other'"))
dat.pusa$W6acqno <- with(dat.pusa, Recode(W6acqno,"c(9) = 'NoAim'"))
dat.pusa$W6acqno<-relevel(dat.pusa$W6acqno,ref="DegreeHE")

# W6GCSENoYP

dat.pusa$W6OwnchiDV <- with(dat.pusa, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.pusa$W6OwnchiDV <- with(dat.pusa, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.pusa$W6OwnchiDV<-relevel(dat.pusa$W6OwnchiDV,ref="No")

dat.pusa$W6OwnchiDV <- with(dat.pusa, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.pusa$W6OwnchiDV <- with(dat.pusa, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.pusa$W6OwnchiDV<-relevel(dat.pusa$W6OwnchiDV,ref="No")

dat.pusa$W8DMARSTAT <- with(dat.pusa, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.pusa$W8DMARSTAT <- with(dat.pusa, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.pusa$W8DMARSTAT <- with(dat.pusa, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.pusa$W8DMARSTAT<-relevel(dat.pusa$W8DMARSTAT,ref="Single")

dat.pusa$W8DACTIVITYC <- with(dat.pusa, Recode(W8DACTIVITYC, "c(1) = 'Employed'"))
dat.pusa$W8DACTIVITYC <- with(dat.pusa, Recode(W8DACTIVITYC, "c(5) = 'Education'"))
dat.pusa$W8DACTIVITYC <- with(dat.pusa, Recode(W8DACTIVITYC, "c(2,3,4,6,7,8,10) = 'Other'"))
dat.pusa$W8DACTIVITYC <- with(dat.pusa, Recode(W8DACTIVITYC, "c(9) = 'LookingAfterHome'"))
dat.pusa$W8DACTIVITYC<-relevel(dat.pusa$W8DACTIVITYC,ref="Employed")

dat.pusa$W8CMSEX <- with(dat.pusa, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.pusa$W8CMSEX <- with(dat.pusa, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.pusa$W8CMSEX<-relevel(dat.pusa$W8CMSEX,ref="Male")

dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(0) = 'NoDegree'"))
dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(1) = 'Degree'"))
dat.pusa$W8DDEGP<-relevel(dat.pusa$W8DDEGP,ref="Degree")

dat.pusa$W8CMSEX <- with(dat.pusa, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.pusa$W8CMSEX <- with(dat.pusa, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.pusa$W8CMSEX<-relevel(dat.pusa$W8CMSEX,ref="Male")

dat.pusa$W8DWRK <- with(dat.pusa, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.pusa$W8DWRK <- with(dat.pusa, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.pusa$W8DWRK<-relevel(dat.pusa$W8DWRK,ref="Employed")

dat.pusa$W8TENURE <- with(dat.pusa, Recode(W8TENURE, "c(2) = 'OwnMortgage'"))
dat.pusa$W8TENURE <- with(dat.pusa, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.pusa$W8TENURE <- with(dat.pusa, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.pusa$W8TENURE <- with(dat.pusa, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.pusa$W8TENURE<-relevel(dat.pusa$W8TENURE,ref="Rent")

dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(1) = 'LivingComfortably'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(2) = 'DoingAlright'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(4) = 'QuiteDifficult'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(5) = 'VeryDifficult'"))
dat.pusa$W8QMAFI<-relevel(dat.pusa$W8QMAFI,ref="LivingComfortably")



pusa.lm <- lm(W8PUSA~ W1yschat1 + W1condur5MP + W1hea2MP 
              + W1InCarHH + W1hous12HH + W1usevcHH + W1hiqualmum + W1wrkfulldad + W1wrkfullmum 
              + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH + W1IndSchool + W1marstatmum  
              + W1nssecfam + W1ethgrpYP + W1heposs9YP + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc
              + W1disabYP + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
              + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DDEGP + W8DGHQSC 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI + W8GROW 
              , data = dat.pusa)

summary(pusa.lm)
vif(pusa.lm)

# Alias predictors 
# W8DWRK and W8DACTIVITYC
summary(lm(W8PUSA ~ W8DWRK   
           , data = dat.pusa))

summary(lm(W8PUSA ~ W8DACTIVITYC   
           , data = dat.pusa))

par(mfrow=c(2,2))
plot(pusa.lm, which=c(1,2))

hist(rstandard(pusa.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

pusa.lm <- lm(log(W8PUSA)~ W1yschat1 + W1condur5MP + W1hea2MP 
              + W1InCarHH + W1hous12HH + W1usevcHH + W1hiqualmum + W1wrkfulldad + W1wrkfullmum 
              + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH + W1IndSchool + W1marstatmum  
              + W1nssecfam + W1ethgrpYP + W1heposs9YP + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc
              + W1disabYP + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
              + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DDEGP + W8DGHQSC 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI + W8GROW 
              , data = dat.pusa)

summary(pusa.lm)

par(mfrow=c(2,2))
plot(pusa.lm, which=c(1,2))

hist(rstandard(pusa.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


show_outliers <- function(the.linear.model, topN) { # length of data
  n = length(fitted(the.linear.model))
  # number of parameters estimated
  p = length(coef(the.linear.model))
  # standardised residuals over 3
  res.out <- which(abs(rstandard(the.linear.model)) > 3) #sometimes >2
  # topN values
  res.top <- head(rev(sort(abs(rstandard(the.linear.model)))), topN)
  # high leverage values
  lev.out <- which(lm.influence(the.linear.model)$hat > 2 * p/n)
  # topN values
  lev.top <- head(rev(sort(lm.influence(the.linear.model)$hat)), topN)
  # high diffits
  dffits.out <- which(dffits(the.linear.model) > 2 * sqrt(p/n))
  # topN values
  dffits.top <- head(rev(sort(dffits(the.linear.model))), topN)
  # Cook's over 1
  cooks.out <- which(cooks.distance(the.linear.model) > 1)
  # topN cooks
  cooks.top <- head(rev(sort(cooks.distance(the.linear.model))), topN)
  # Create a list with the statistics -- cant do a data frame as different
  # lengths
  list.of.stats <- list(Std.res = res.out, Std.res.top = res.top, 
                        Leverage = lev.out, Leverage.top = lev.top, 
                        DFFITS = dffits.out, DFFITS.top = dffits.top,
                        Cooks = cooks.out, Cooks.top = cooks.top) # return the statistics
  list.of.stats
}


pusa.lm.stats<-show_outliers(pusa.lm, 5)
pusa.lm.stats

outliers <- Reduce(intersect,list(pusa.lm.stats$Std.res, pusa.lm.stats$DFFITS))
outliers.1 <- outliers
dat.pusa[outliers.1,]

outliers <- Reduce(intersect,list(pusa.lm.stats$Leverage, pusa.lm.stats$Std.res))
outliers.2 <- outliers
dat.pusa[outliers.2,]


outliers <- Reduce(intersect,list(pusa.lm.stats$Leverage, pusa.lm.stats$DFFITS))
outliers.3 <- outliers
dat.pusa[outliers.3,]

all.outliers <- union(union(outliers.1, outliers.2), outliers.3)
dat.pusa[all.outliers,]

# Comparing the regression models
no.outlier.dat <-dat.pusa[-all.outliers,]

no.outlier.dat.lm <- lm(log(W8PUSA)~ W1yschat1 + W1condur5MP + W1hea2MP 
                        + W1InCarHH + W1hous12HH + W1usevcHH + W1hiqualmum + W1wrkfulldad + W1wrkfullmum 
                        + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH + W1IndSchool + W1marstatmum  
                        + W1nssecfam + W1ethgrpYP + W1heposs9YP + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc
                        + W1disabYP + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
                        + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DDEGP + W8DGHQSC 
                        + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI + W8GROW, data = no.outlier.dat)

summary(no.outlier.dat.lm)

par(mfrow=c(2,2))

plot(no.outlier.dat.lm, which=c(1,2))
hist(rstandard(no.outlier.dat.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


# proceed with original as there is no reason to justify removing outliers
# removed all the non significant predictors at the 5% level
pusa.lm <- lm(log(W8PUSA)~ W8DDEGP + W8DGHQSC + W8QMAFI 
              , data = dat.pusa)

summary(pusa.lm)

par(mfrow=c(2,2))
plot(pusa.lm, which=c(1,2))

hist(rstandard(pusa.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(pusa.lm)

# Removing W8DGHQSC because no longer significant with a larger sample size
pusa.lm <- lm(log(W8PUSA)~ W8DDEGP + W8QMAFI 
              , data = dat.pusa)

summary(pusa.lm)

par(mfrow=c(2,2))
plot(pusa.lm, which=c(1,2))

hist(rstandard(pusa.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(pusa.lm)

# Adding originally removed one by one predictors back to see if there is a change
pusa.lm <- lm(log(W8PUSA)~ W8DDEGP + W8QMAFI + W1famtyp2 + W6EducYP 
              + W6Apprent1YP + W1wrk1aMP + W6acqno + W1NoldBroHS + W8DACTIVITYC + W8DAGEYCH
              , data = dat.pusa)

vif(pusa.lm)

summary(pusa.lm)

# Found that W1wrk1aMP is still significant, removed 1 by 1
pusa.lm <- lm(log(W8PUSA)~ W8DDEGP + W8QMAFI + W1wrk1aMP
              , data = dat.pusa)

vif(pusa.lm)

summary(pusa.lm)

Anova(pusa.lm)


# Trying out interactions
pusa.lm <- lm(log(W8PUSA)~ W8DDEGP*W8QMAFI + W1wrk1aMP 
              , data = dat.pusa)

summary(pusa.lm)
Anova(pusa.lm)

pusa.lm <- lm(log(W8PUSA)~ W8DDEGP + W8QMAFI*W1wrk1aMP
              , data = dat.pusa)

summary(pusa.lm)
Anova(pusa.lm)

pusa.lm <- lm(log(W8PUSA)~ W8DDEGP*W1wrk1aMP + W8QMAFI 
              , data = dat.pusa)

summary(pusa.lm)
Anova(pusa.lm)


# Final Model
pusa.lm <- lm(log(W8PUSA)~ W1wrk1aMP + W8DDEGP + W8QMAFI 
              , data = dat.pusa)

summary(pusa.lm)

Anova(pusa.lm)

par(mfrow=c(2,2))
plot(pusa.lm, which=c(1,2))

hist(rstandard(pusa.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(pusa.lm)

# Missing Data
# Replacing with missing
dat.pusa <- read.csv("EOTST2112023_PUSA.csv", header = TRUE)
dat.pusa <- dat.pusa[, -1]
dat.pusa <- subset(dat.pusa, W8PUSA >= 0) 
rownames(dat.pusa) <- 1:nrow(dat.pusa)
dat.pusa$W8DGHQSC[dat.pusa$W8DGHQSC < 0] <- NA
dat.pusa[dat.pusa <= -1] <- "Missing"
dat.pusa$W8PUSA[dat.pusa$W8PUSA==0]<- 0.05
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,
          32,33,34,35,36,37,38,40,43,44,45,46,47,48)
dat.pusa[cols] <- lapply(dat.pusa[cols], factor)

dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(0) = 'NoDegree'"))
dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(1) = 'Degree'"))
dat.pusa$W8DDEGP<-relevel(dat.pusa$W8DDEGP,ref="Degree")

dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(1,2) = 'AboveAverage'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(3) = 'Average'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(4,5) = 'BelowAverage'"))
dat.pusa$W8QMAFI<-relevel(dat.pusa$W8QMAFI,ref="AboveAverage")

dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(1,3) = 'FullTime'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(2,4) = 'PartTime'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(5,6,7,8,9,11,12) = 'Other'"))
dat.pusa$W1wrk1aMP<-relevel(dat.pusa$W1wrk1aMP,ref="FullTime")

pusa.lm <- lm(log(W8PUSA)~ W1wrk1aMP + W8DDEGP + W8QMAFI 
              , data = dat.pusa)

summary(pusa.lm)

par(mfrow=c(2,2))
plot(pusa.lm, which=c(1,2))

hist(rstandard(pusa.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

# Didn't change negative to missing

dat.pusa <- read.csv("EOTST2112023_PUSA.csv", header = TRUE)
dat.pusa <- dat.pusa[, -1]
dat.pusa <- subset(dat.pusa, W8PUSA >= 0) 
dat.pusa$W8DGHQSC[dat.pusa$W8DGHQSC < 0] <- NA
rownames(dat.pusa) <- 1:nrow(dat.pusa)
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,
          32,33,34,35,36,37,38,40,43,44,45,46,47,48)
dat.pusa[cols] <- lapply(dat.pusa[cols], factor)
dat.pusa$W8PUSA[dat.pusa$W8PUSA==0]<- 0.05

dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(0) = 'NoDegree'"))
dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(1) = 'Degree'"))
dat.pusa$W8DDEGP<-relevel(dat.pusa$W8DDEGP,ref="Degree")

dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(1,2) = 'AboveAverage'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(3) = 'Average'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(4,5) = 'Below'"))

dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(1,3) = 'FullTime'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(2,4) = 'PartTime'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(5,6,7,8,9,11,12) = 'Other'"))
dat.pusa$W1wrk1aMP<-relevel(dat.pusa$W1wrk1aMP,ref="FullTime")

pusa.lm <- lm(log(W8PUSA)~ W1wrk1aMP + W8DDEGP + W8QMAFI 
              , data = dat.pusa)

summary(pusa.lm)

par(mfrow=c(2,2))
plot(pusa.lm, which=c(1,2))

hist(rstandard(pusa.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


# Cross Validation for different splits
dat.pusa <- read.csv("EOTST2112023_PUSA.csv", header = TRUE)
dat.pusa <- dat.pusa[, -1]
dat.pusa <- subset(dat.pusa, W8PUSA >= 0) 
dat.pusa$W8PUSA[dat.pusa$W8PUSA==0]<- 0.05
rownames(dat.pusa) <- 1:nrow(dat.pusa)
dat.pusa[dat.pusa <= -1] <- NA
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,
          32,33,34,35,36,37,38,40,43,44,45,46,47,48)
dat.pusa[cols] <- lapply(dat.pusa[cols], factor)
dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(0) = 'NoDegree'"))
dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(1) = 'Degree'"))
dat.pusa$W8DDEGP<-relevel(dat.pusa$W8DDEGP,ref="Degree")

dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(1,2) = 'AboveAverage'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(3) = 'Average'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(4,5) = 'Below'"))

dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(1,3) = 'FullTime'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(2,4) = 'PartTime'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(5,6,7,8,9,11,12) = 'Other'"))
dat.pusa$W1wrk1aMP<-relevel(dat.pusa$W1wrk1aMP,ref="FullTime")

for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.pusa),split.proportions[i]*nrow(dat.pusa) , replace=FALSE)
  training.set<-dat.pusa[cross.val,] #the 50/70/90% to fit the model
  test.set<-dat.pusa[-cross.val,] # the 50/30/10% to use as validation sample
  #fit the model
  cv.pusa.lm<- pusa.lm <- lm(log(W8PUSA)~ W1wrk1aMP + W8DDEGP + W8QMAFI 
                             , data = dat.pusa)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.pusa.lm,test.set), 
                           #predicted vs original
                           original=test.set$W8PUSA,error=(predict(cv.pusa.lm,test.set)-test.set$W8PUSA))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", se=FALSE) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)



split.proportions<-c(0.7,0.8,0.9)
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.pusa),split.proportions[i]*nrow(dat.pusa) , replace=FALSE)
  training.set<-dat.pusa[cross.val,] #the 50/70/90% to fit the model
  test.set<-dat.pusa[-cross.val,] # the 50/30/10% to use as validation sample
  #fit the model
  pusa.lm <- lm(log(W8PUSA)~ W1wrk1aMP + W8DDEGP + W8QMAFI 
                , data = dat.pusa)
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(pusa.lm,test.set), 
                           #predicted vs original
                           original=test.set$W8PUSA,error=(predict(pusa.lm,test.set)-test.set$W8PUSa))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", se=FALSE) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)


# Cross validation with rich people removed

dat.pusa <- read.csv("EOTST2112023_PUSA.csv", header = TRUE)
dat.pusa <- dat.pusa[, -1]
dat.pusa <- subset(dat.pusa, W8PUSA >= 0 & W8PUSA < 35000) 
dat.pusa$W8PUSA[dat.pusa$W8PUSA==0]<- 0.05
rownames(dat.pusa) <- 1:nrow(dat.pusa)
dat.pusa[dat.pusa <= -1] <- NA
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,30,31,
          32,33,34,35,36,37,38,40,43,44,45,46,47,48)

dat.pusa[cols] <- lapply(dat.pusa[cols], factor)
dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(0) = 'NoDegree'"))
dat.pusa$W8DDEGP <- with(dat.pusa, Recode(W8DDEGP, "c(1) = 'Degree'"))
dat.pusa$W8DDEGP<-relevel(dat.pusa$W8DDEGP,ref="Degree")

dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(1,2) = 'AboveAverage'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(3) = 'Average'"))
dat.pusa$W8QMAFI <- with(dat.pusa, Recode(W8QMAFI, "c(4,5) = 'Below'"))

dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(1,3) = 'FullTime'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP, "c(2,4) = 'PartTime'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.pusa$W1wrk1aMP <- with(dat.pusa, Recode(W1wrk1aMP,"c(5,6,7,8,9,11,12) = 'Other'"))
dat.pusa$W1wrk1aMP<-relevel(dat.pusa$W1wrk1aMP,ref="FullTime")

for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.pusa),split.proportions[i]*nrow(dat.pusa) , replace=FALSE)
  training.set<-dat.pusa[cross.val,] #the 50/70/90% to fit the model
  test.set<-dat.pusa[-cross.val,] # the 50/30/10% to use as validation sample
  #fit the model
  cv.pusa.lm<- pusa.lm <- lm(log(W8PUSA)~ W1wrk1aMP + W8DDEGP + W8QMAFI 
                             , data = dat.pusa)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.pusa.lm,test.set), 
                           #predicted vs original
                           original=test.set$W8PUSA,error=(predict(cv.pusa.lm,test.set)-test.set$W8PUSA))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", se=FALSE) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)



split.proportions<-c(0.7,0.8,0.9)
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.pusa),split.proportions[i]*nrow(dat.pusa) , replace=FALSE)
  training.set<-dat.pusa[cross.val,] #the 50/70/90% to fit the model
  test.set<-dat.pusa[-cross.val,] # the 50/30/10% to use as validation sample
  #fit the model
  pusa.lm <- lm(log(W8PUSA)~ W1wrk1aMP + W8DDEGP + W8QMAFI 
                , data = dat.pusa)
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(pusa.lm,test.set), 
                           #predicted vs original
                           original=test.set$W8PUSA,error=(predict(pusa.lm,test.set)-test.set$W8PUSA))
  #first iteration
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    #regress one on the other to see "fit"
    p1<-p1+geom_smooth(method="lm", se=FALSE) 
    #the ideal would be for the lm to fit the diagonal
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    #predicted vs error
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    #points for the second iteration  
    if(i==2){
      
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkred") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      #points for the third iteration
      #points on LHS plot
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      #regress one on the other to see "fit"
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkgreen") 
      
      #points on RHS plot
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      #lines at 0 and +/- one std deviation of error
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)


