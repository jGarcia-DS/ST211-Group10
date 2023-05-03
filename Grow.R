library(arm)
library(ggplot2)
library(car)
library(tidyr)
library(dplyr)
library(gridExtra)

options(max.print=9999999) # So we can increase the amount that is printed in R

dat.grow <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
# Removing NSID
dat.grow <- dat.grow[, -1]
# Making sure that the outcome is fully observed
dat.grow <- subset(dat.grow, W8GROW >= 0) 
dat.grow$W8GROW[dat.grow$W8GROW==0]<- 0.05
rownames(dat.grow) <- 1:nrow(dat.grow)
dat.grow[dat.grow <= -1] <- NA

summary(dat.grow)

# Converting columns to categorical predictors using factor
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow[cols] <- lapply(dat.grow[cols], factor)


dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(1) = 'FullTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(2) = 'PartTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.grow$W1wrk1aMP<-relevel(dat.grow$W1wrk1aMP,ref="Other")

dat.grow$W1condur5MP <- with(dat.grow, Recode(W1condur5MP, "c(1) = 'Yes'"))
dat.grow$W1condur5MP <- with(dat.grow, Recode(W1condur5MP, "c(2) = 'No'"))
dat.grow$W1condur5MP<-relevel(dat.grow$W1condur5MP,ref="Yes")

dat.grow$W1hea2MP <- with(dat.grow, Recode(W1hea2MP, "c(1) = 'Yes'"))
dat.grow$W1hea2MP <- with(dat.grow, Recode(W1hea2MP, "c(2) = 'No'"))
dat.grow$W1hea2MP<-relevel(dat.grow$W1hea2MP,ref="No")

dat.grow$W1NoldBroHS <- with(dat.grow, Recode(W1NoldBroHS, "c(0) = 0"))
dat.grow$W1NoldBroHS <- with(dat.grow, Recode(W1NoldBroHS, "c(1) = 1"))
dat.grow$W1NoldBroHS <- with(dat.grow, Recode(W1NoldBroHS, "c(2) = 2"))
dat.grow$W1NoldBroHS <- with(dat.grow, Recode(W1NoldBroHS, "c(3,4,5,6,7,8,9) = '>3'"))
dat.grow$W1NoldBroHS<-relevel(dat.grow$W1NoldBroHS,ref='0')

# No need to merge W1InCarHH

# Based on sample size
dat.grow$W1hous12HH <- with(dat.grow, Recode(W1hous12HH, "c(1) = 'OwnedOutright'"))
dat.grow$W1hous12HH <- with(dat.grow, Recode(W1hous12HH, "c(2) = 'BeingBought'"))
dat.grow$W1hous12HH <- with(dat.grow, Recode(W1hous12HH, "c(3,4,5,6,7,8) = 'Other'"))
dat.grow$W1hous12HH<-relevel(dat.grow$W1hous12HH,ref='Other')

dat.grow$W1hiqualdad <- with(dat.grow, Recode(W1hiqualdad, "c(2) = 'FirstDegree'"))
dat.grow$W1hiqualdad <- with(dat.grow, Recode(W1hiqualdad, "c(4) = 'HNC/HND/NVQ4'"))
dat.grow$W1hiqualdad <- with(dat.grow, Recode(W1hiqualdad, "c(13) = 'TradeApprenticeship'"))
dat.grow$W1hiqualdad <- with(dat.grow, Recode(W1hiqualdad, "c(15) = 'GCSEa-c'"))
dat.grow$W1hiqualdad <- with(dat.grow, Recode(W1hiqualdad, "c(20) = 'NoQualMentioned'"))
dat.grow$W1hiqualdad <- with(dat.grow, Recode(W1hiqualdad, "c(1,3,5,6,7,8,9,10,11,12,14,16,17,18,19) = 'Other'"))
dat.grow$W1hiqualdad<-relevel(dat.grow$W1hiqualdad,ref='Other')

dat.grow$W1wrkfulldad <- with(dat.grow, Recode(W1wrkfulldad, "c(1) = 'FullTime'"))
dat.grow$W1wrkfulldad <- with(dat.grow, Recode(W1wrkfulldad, "c(2) = 'PartTime'"))
dat.grow$W1wrkfulldad <- with(dat.grow, Recode(W1wrkfulldad, "c(3) = 'NotWorking'"))
dat.grow$W1wrkfulldad<-relevel(dat.grow$W1wrkfulldad,ref='FullTime')

dat.grow$W1wrkfullmum <- with(dat.grow, Recode(W1wrkfullmum, "c(1) = 'FullTime'"))
dat.grow$W1wrkfullmum <- with(dat.grow, Recode(W1wrkfullmum, "c(2) = 'PartTime'"))
dat.grow$W1wrkfullmum <- with(dat.grow, Recode(W1wrkfullmum, "c(3) = 'NotWorking'"))
dat.grow$W1wrkfullmum<-relevel(dat.grow$W1wrkfullmum,ref='FullTime')

dat.grow$W1empsdad <- with(dat.grow, Recode(W1empsdad, "c(1) = 'PaidWork>30h'"))
dat.grow$W1empsdad <- with(dat.grow, Recode(W1empsdad, "c(2) = 'PaidWork<30h'"))
dat.grow$W1empsdad <- with(dat.grow, Recode(W1empsdad, "c(8) = 'Sick/Disabled'"))
dat.grow$W1empsdad <- with(dat.grow, Recode(W1empsdad, "c(3,4,5,6,7,9) = 'Other'"))
dat.grow$W1empsdad<-relevel(dat.grow$W1empsdad,ref='PaidWork>30h')

dat.grow$W1ch0_2HH <- with(dat.grow, Recode(W1ch0_2HH, "c(0) = '0'"))
dat.grow$W1ch0_2HH <- with(dat.grow, Recode(W1ch0_2HH, "c(1) = '1'"))
dat.grow$W1ch0_2HH <- with(dat.grow, Recode(W1ch0_2HH, "c(2,3) = '>1'"))
dat.grow$W1ch0_2HH<-relevel(dat.grow$W1ch0_2HH,ref='0')

dat.grow$W1ch3_11HH <- with(dat.grow, Recode(W1ch3_11HH, "c(0) = '0'"))
dat.grow$W1ch3_11HH <- with(dat.grow, Recode(W1ch3_11HH, "c(1) = '1'"))
dat.grow$W1ch3_11HH <- with(dat.grow, Recode(W1ch3_11HH, "c(2,3,4,5) = '>1'"))
dat.grow$W1ch3_11HH<-relevel(dat.grow$W1ch3_11HH,ref='0')

dat.grow$W1ch12_15HH <- with(dat.grow, Recode(W1ch12_15HH, "c(0) = '0'"))
dat.grow$W1ch12_15HH <- with(dat.grow, Recode(W1ch12_15HH, "c(1) = '1'"))
dat.grow$W1ch12_15HH <- with(dat.grow, Recode(W1ch12_15HH, "c(2,3,4) = '>1'"))
dat.grow$W1ch12_15HH<-relevel(dat.grow$W1ch12_15HH,ref='0')

dat.grow$W1ch16_17HH <- with(dat.grow, Recode(W1ch16_17HH, "c(0) = '0'"))
dat.grow$W1ch16_17HH <- with(dat.grow, Recode(W1ch16_17HH, "c(1) = '1'"))
dat.grow$W1ch16_17HH <- with(dat.grow, Recode(W1ch16_17HH, "c(2,3,4) = '>1'"))
dat.grow$W1ch16_17HH<-relevel(dat.grow$W1ch16_17HH,ref='0')

# W1IndSchool not changing

dat.grow$W1marstatmum <- with(dat.grow, Recode(W1marstatmum, "c(2) = 'Married'"))
dat.grow$W1marstatmum <- with(dat.grow, Recode(W1marstatmum, "c(5) = 'Divorced'"))
dat.grow$W1marstatmum <- with(dat.grow, Recode(W1marstatmum, "c(1,3,4,6,7) = 'Other'"))
dat.grow$W1marstatmum<-relevel(dat.grow$W1marstatmum,ref='Other')

dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(1) = 'HigherManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(2) = 'LowerManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(4) = 'SmallEmployers'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(3,5,6,7,8) = 'Other'"))
dat.grow$W1nssecfam<-relevel(dat.grow$W1nssecfam,ref='HigherManagerial/Professional')

dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W1heposs9YP <- with(dat.grow, Recode(W1heposs9YP, "c(1,2) = 'Likely'"))
dat.grow$W1heposs9YP <- with(dat.grow, Recode(W1heposs9YP, "c(3,4) = 'NotLikely'"))
dat.grow$W1heposs9YP<-relevel(dat.grow$W1heposs9YP,ref="Likely")

dat.grow$W1hwndayYP <- with(dat.grow, Recode(W1hwndayYP, "c(0,1) = '0-1'"))
dat.grow$W1hwndayYP <- with(dat.grow, Recode(W1hwndayYP, "c(2,3) = '2-3'"))
dat.grow$W1hwndayYP <- with(dat.grow, Recode(W1hwndayYP,"c(4,5) = '4-5'"))
dat.grow$W1hwndayYP<-relevel(dat.grow$W1hwndayYP,ref="0-1")

# Not dealing with W1truantYP

# same as W1alceverYP

# W1bulrc

# W1disabYP

# W2disc1YP

# W2depressYP

# W6JobYP

dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(1) = 'Yes'"))
dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(2) = 'No'"))
dat.grow$W6UnivYP<-relevel(dat.grow$W6UnivYP,ref="No")

# W6EducYP

# W6Apprent1YP

dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno, "c(1) = 'DegreeHE'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno, "c(3,4) = 'A-levels'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno,"c(5,6) = 'GCSE'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno,"c(7,8) = 'Other'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno,"c(9) = 'NoAim'"))
dat.grow$W6acqno<-relevel(dat.grow$W6acqno,ref="DegreeHE")

# W6GCSENoYP

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

# W8DDEGP

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W8DACTIVITYC <- with(dat.grow, Recode(W8DACTIVITYC, "c(1) = 'Employed'"))
dat.grow$W8DACTIVITYC <- with(dat.grow, Recode(W8DACTIVITYC, "c(5) = 'Education'"))
dat.grow$W8DACTIVITYC <- with(dat.grow, Recode(W8DACTIVITYC, "c(2,3,4,6,7,8,10) = 'Other'"))
dat.grow$W8DACTIVITYC <- with(dat.grow, Recode(W8DACTIVITYC, "c(9) = 'LookingAfterHome'"))
dat.grow$W8DACTIVITYC<-relevel(dat.grow$W8DACTIVITYC,ref="Employed")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(2) = 'OwnMortgage'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1) = 'LivingComfortably'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(2) = 'DoingAlright'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4) = 'QuiteDifficult'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(5) = 'VeryDifficult'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="LivingComfortably")

## Needed to be below 1000 to have a good look at all the boxplots
dat.grow.norich<-subset(dat.grow,W8GROW<=1000)

# continuous data scatterplots
special.dat <- gather(data = dat.grow.norich[, c(1, 2, 29, 40, 42, 49)], -W8GROW, key = "var", value = "value")
p1 <- ggplot(special.dat, aes(x = value, y = W8GROW)) + geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~var, scales = "free_x")
p1 <- p1 + theme_bw() + scale_fill_grey() + theme(legend.position = "none")
p1

# categorical data boxplots
special.dat <- gather(data = dat.grow.norich[, -c(1, 2, 29, 40, 42)], -W8GROW, key = "var", value = "value")
p2 <- ggplot(special.dat, aes(x = factor(value), y = W8GROW)) + geom_point(size = 0.5) +
  geom_boxplot() + facet_wrap(~var, scales = "free_x")

p2 <- p2 + theme_bw() + scale_fill_grey() + theme(legend.position = "none")
p2


# Calculate the proportion of continuous containing more than 30%
proportion <- sum(is.na(dat.grow$W1GrssyrMP)) / length(dat.grow$W1GrssyrMP)
proportion

proportion <- sum(is.na(dat.grow$W1yschat1)) / length(dat.grow$W1yschat1)
proportion

proportion <- sum(is.na(dat.grow$W2ghq12scr)) / length(dat.grow$W2ghq12scr)
proportion

proportion <- sum(is.na(dat.grow$W6DebtattYP)) / length(dat.grow$W6DebtattYP)
proportion

proportion <- sum(is.na(dat.grow$W8DGHQSC)) / length(dat.grow$W8DGHQSC)
proportion

# make a liner model without W1GrssyrMP
grow.lm <- lm(W8GROW ~ W1yschat1 + W1condur5MP + W1hea2MP 
              + W1InCarHH + W1hous12HH + W1usevcHH  + W1hiqualdad + W1wrkfulldad
              + W1wrkfullmum + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH
              + W1IndSchool + W1marstatmum + W1nssecfam  + W1ethgrpYP + W1heposs9YP
              + W1hwndayYP + W1truantYP + W1alceverYP  + W1bulrc + W1disabYP 
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP  
              + W6gcse + W6OwnchiDV + W6DebtattYP + W6als
              + W8DDEGP + W8DGHQSC + W8DMARSTAT + W8DWRK 
              + W8CMSEX + W8TENURE + W8QMAFI 
              , data = dat.grow)

vif(grow.lm)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


### for W1hiqualdad  W1wrkfulldad  W1empsdad
summary(lm(W8GROW ~ W1hiqualdad   
            , data = dat.grow))
 
summary(lm(W8GROW ~ W1wrkfulldad   
            , data = dat.grow))
 
summary(lm(W8GROW ~ W1empsdad
            , data = dat.grow))
 
## For W8DACTIVITYC, W8DWRK

summary(lm(W8GROW ~ W8DWRK   
           , data = dat.grow))

summary(lm(W8GROW ~ W8DACTIVITYC   
           , data = dat.grow))



## Linear models of predictors that we removed 
summary(lm(W8GROW ~ W1GrssyrMP, data = dat.grow))

summary(lm(W8GROW ~ W6acqno, data = dat.grow))

summary(lm(W8GROW ~ W1wrk1aMP, data = dat.grow))

summary(lm(W8GROW ~ W1NoldBroHS, data = dat.grow))

summary(lm(W8GROW ~ W1wrkfullmum, data = dat.grow))

## log transform
grow.lm <- lm(log(W8GROW) ~ W1yschat1 + W1condur5MP + W1hea2MP 
              + W1InCarHH + W1hous12HH + W1usevcHH  + W1hiqualdad + W1wrkfulldad
              + W1wrkfullmum + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH
              + W1IndSchool + W1marstatmum + W1nssecfam  + W1ethgrpYP + W1heposs9YP
              + W1hwndayYP + W1truantYP + W1alceverYP  + W1bulrc + W1disabYP 
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP  
              + W6gcse + W6OwnchiDV + W6DebtattYP + W6als
              + W8DDEGP + W8DGHQSC + W8DMARSTAT + W8DWRK
              + W8CMSEX + W8TENURE + W8QMAFI
              , data = dat.grow)
summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
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


grow.lm.stats<-show_outliers(grow.lm, 5)
grow.lm.stats


outliers <- Reduce(intersect,list(grow.lm.stats$Std.res, grow.lm.stats$DFFITS))

outliers.1 <- outliers
dat.grow[outliers.1,]

outliers <- Reduce(intersect,list(grow.lm.stats$Leverage, grow.lm.stats$Std.res))
outliers.2 <- outliers
dat.grow[outliers.2,]


outliers <- Reduce(intersect,list(grow.lm.stats$Leverage, grow.lm.stats$DFFITS))
outliers.3 <- outliers
dat.grow[outliers.3,]

all.outliers <- union(union(outliers.1, outliers.2), outliers.3)
dat.grow[all.outliers,]


## outliers 
no.outlier.dat <-dat.grow[-all.outliers,]

no.outlier.dat.lm <- lm(log(W8GROW) ~ W1yschat1 + W1condur5MP + W1hea2MP 
                        + W1InCarHH + W1hous12HH + W1usevcHH  + W1hiqualdad + W1wrkfulldad
                        + W1wrkfullmum + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH
                        + W1IndSchool + W1marstatmum + W1nssecfam  + W1ethgrpYP + W1heposs9YP
                        + W1hwndayYP + W1truantYP + W1alceverYP  + W1bulrc + W1disabYP 
                        + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP  
                        + W6gcse + W6OwnchiDV + W6DebtattYP + W6als
                        + W8DDEGP + W8DGHQSC + W8DMARSTAT + W8DWRK
                        + W8CMSEX + W8TENURE + W8QMAFI , data = no.outlier.dat)
summary(no.outlier.dat.lm)


par(mfrow=c(2,2))

plot(no.outlier.dat.lm, which=c(1,2))
hist(rstandard(no.outlier.dat.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


# Proceeded with original dataset
grow.lm <- lm(log(W8GROW) ~ W1yschat1 + W1condur5MP + W1hea2MP 
              + W1InCarHH + W1hous12HH + W1usevcHH  + W1hiqualdad + W1wrkfulldad
              + W1wrkfullmum + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH
              + W1IndSchool + W1marstatmum + W1nssecfam  + W1ethgrpYP + W1heposs9YP
              + W1hwndayYP + W1truantYP + W1alceverYP  + W1bulrc + W1disabYP 
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP  
              + W6gcse + W6OwnchiDV + W6DebtattYP + W6als
              + W8DDEGP + W8DGHQSC + W8DMARSTAT + W8DWRK
              + W8CMSEX + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

Anova(grow.lm)


## Removing the non-significant predictors at the 5% level
grow.lm <- lm(log(W8GROW) ~ W1nssecfam + W1ethgrpYP + W6UnivYP + W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(grow.lm)

# Adding back the predictors we initially removed to see if there is anything interesting about the final model
grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1ethgrpYP + W6UnivYP + W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI
              + W1empsdad + W1wrk1aMP + W1NoldBroHS 
              + W1wrkfullmum + W6acqno + W1GrssyrMP
              , data = dat.grow)

summary(grow.lm)

vif(grow.lm)

# removing the non-significant predictors that we added back
grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1ethgrpYP + W6UnivYP + W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI
              + W1wrk1aMP 
              , data = dat.grow)

summary(grow.lm)

Anova(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(grow.lm)


# Interactions
grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1ethgrpYP + W6UnivYP*W8TENURE + W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8QMAFI
              + W1wrk1aMP 
              , data = dat.grow)

summary(grow.lm)
Anova(grow.lm)

grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1ethgrpYP + W6UnivYP*W8TENURE + W6OwnchiDV 
              + W8DMARSTAT + W8DWRK*W8CMSEX + W8QMAFI
              + W1wrk1aMP 
              , data = dat.grow)


summary(grow.lm)
Anova(grow.lm)

grow.lm <- lm(log(W8GROW) ~  W1nssecfam*W6UnivYP + W1ethgrpYP + W6UnivYP*W8TENURE + W6OwnchiDV 
              + W8DMARSTAT + W8DWRK*W8CMSEX + W8QMAFI
              + W1wrk1aMP 
              , data = dat.grow)


summary(grow.lm)
Anova(grow.lm)

grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W6UnivYP*W8TENURE + W6OwnchiDV*W1ethgrpYP
              + W8DMARSTAT + W8DWRK*W8CMSEX + W8QMAFI
              + W1wrk1aMP 
              , data = dat.grow)


summary(grow.lm)
Anova(grow.lm)

grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1ethgrpYP + W6UnivYP*W8TENURE + W6OwnchiDV
              + W8DMARSTAT*W8QMAFI + W8DWRK*W8CMSEX
              + W1wrk1aMP 
              , data = dat.grow)


summary(grow.lm)
Anova(grow.lm)

grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1ethgrpYP + W6UnivYP*W8TENURE + W6OwnchiDV
              + W8DMARSTAT*W8DWRK + W8DWRK*W8CMSEX + W8QMAFI
              + W1wrk1aMP 
              , data = dat.grow)


summary(grow.lm)
Anova(grow.lm)

grow.lm <- lm(log(W8GROW) ~  W1nssecfam*W1wrk1aMP + W1ethgrpYP + W6UnivYP*W8TENURE + W6OwnchiDV
              + W8DMARSTAT + W8DWRK*W8CMSEX + W8QMAFI
              , data = dat.grow)


summary(grow.lm)
Anova(grow.lm)

grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1wrk1aMP + W1ethgrpYP + W6UnivYP*W8TENURE 
              + W8DMARSTAT*W6OwnchiDV + W8DWRK*W8CMSEX + W8QMAFI
              , data = dat.grow)


summary(grow.lm)
Anova(grow.lm)

# FINAL MODEL
grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1wrk1aMP + W1ethgrpYP + W6UnivYP*W8TENURE 
              + W6OwnchiDV + W8DWRK*W8CMSEX + W8QMAFI + W8DMARSTAT
              , data = dat.grow)

summary(grow.lm)

Anova(grow.lm)

# Missing Data
dat.grow <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
dat.grow <- dat.grow[, -1]
dat.grow <- subset(dat.grow, W8GROW > 0) 
rownames(dat.grow) <- 1:nrow(dat.grow)
dat.grow[dat.grow <= -1] <- "Missing"
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow[cols] <- lapply(dat.grow[cols], factor)


dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(1) = 'HigherManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(2) = 'LowerManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(4) = 'SmallEmployers'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(3,5,6,7,8) = 'Other'"))
dat.grow$W1nssecfam<-relevel(dat.grow$W1nssecfam,ref='HigherManagerial/Professional')

dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(1) = 'FullTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(2) = 'PartTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.grow$W1wrk1aMP<-relevel(dat.grow$W1wrk1aMP,ref="Other")

dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(1) = 'Yes'"))
dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(2) = 'No'"))
dat.grow$W6UnivYP<-relevel(dat.grow$W6UnivYP,ref="No")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(2) = 'OwnMortgage'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1) = 'LivingComfortably'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(2) = 'DoingAlright'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4) = 'QuiteDifficult'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(5) = 'VeryDifficult'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="LivingComfortably")

grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1wrk1aMP + W1ethgrpYP + W6UnivYP*W8TENURE 
              + W6OwnchiDV + W8DWRK*W8CMSEX + W8QMAFI + W8DMARSTAT
              , data = dat.grow)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(grow.lm)


# original without changing the negative to "Missing"
dat.grow <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
dat.grow <- dat.grow[, -1]
dat.grow <- subset(dat.grow, W8GROW > 0) 
rownames(dat.grow) <- 1:nrow(dat.grow)
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow[cols] <- lapply(dat.grow[cols], factor)

dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(1) = 'HigherManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(2) = 'LowerManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(4) = 'SmallEmployers'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(3,5,6,7,8) = 'Other'"))
dat.grow$W1nssecfam<-relevel(dat.grow$W1nssecfam,ref='HigherManagerial/Professional')

dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(1) = 'FullTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(2) = 'PartTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.grow$W1wrk1aMP<-relevel(dat.grow$W1wrk1aMP,ref="Other")

dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(1) = 'Yes'"))
dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(2) = 'No'"))
dat.grow$W6UnivYP<-relevel(dat.grow$W6UnivYP,ref="No")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(2) = 'OwnMortgage'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1) = 'LivingComfortably'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(2) = 'DoingAlright'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4) = 'QuiteDifficult'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(5) = 'VeryDifficult'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="LivingComfortably")

grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1wrk1aMP + W1ethgrpYP + W6UnivYP*W8TENURE 
              + W6OwnchiDV + W8DWRK*W8CMSEX + W8QMAFI + W8DMARSTAT
              , data = dat.grow)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(grow.lm)


# Cross Validation
dat.grow <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
dat.grow <- dat.grow[, -1]
dat.grow <- subset(dat.grow, W8GROW >= 0) 
dat.grow$W8GROW[dat.grow$W8GROW==0]<- 0.05
dat.grow[dat.grow <= -1] <- NA
rownames(dat.grow) <- 1:nrow(dat.grow)
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow[cols] <- lapply(dat.grow[cols], factor)

dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(1) = 'HigherManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(2) = 'LowerManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(4) = 'SmallEmployers'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(3,5,6,7,8) = 'Other'"))
dat.grow$W1nssecfam<-relevel(dat.grow$W1nssecfam,ref='HigherManagerial/Professional')

dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(1) = 'FullTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(2) = 'PartTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.grow$W1wrk1aMP<-relevel(dat.grow$W1wrk1aMP,ref="Other")

dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(1) = 'Yes'"))
dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(2) = 'No'"))
dat.grow$W6UnivYP<-relevel(dat.grow$W6UnivYP,ref="No")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(2) = 'OwnMortgage'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1) = 'LivingComfortably'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(2) = 'DoingAlright'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4) = 'QuiteDifficult'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(5) = 'VeryDifficult'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="LivingComfortably")


for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.grow),0.8*nrow(dat.grow) , replace=FALSE)
  training.set<-dat.grow[cross.val,] #the 80% to fit the model
  test.set<-dat.grow[-cross.val,] # the 20% to use as validation sample
  #fit the model
  grow.lm <- grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1wrk1aMP + W1ethgrpYP + W6UnivYP*W8TENURE 
                           + W6OwnchiDV + W8DWRK*W8CMSEX + W8QMAFI + W8DMARSTAT
                           , data = dat.grow)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(grow.lm,test.set), 
                           #predicted vs original
                           original=test.set$W8GROW,error=(predict(grow.lm,test.set)-test.set$W8GROW))
  
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

# Cross validation but the rich people are removed, just to see if our model is good 
# at predicting the weekly gross income of an average person
dat.grow <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
dat.grow <- dat.grow[, -1]
dat.grow <- subset(dat.grow, W8GROW >= 0 & W8GROW < 4000) 
dat.grow$W8GROW[dat.grow$W8GROW==0]<- 0.05
dat.grow[dat.grow <= -1] <- NA
rownames(dat.grow) <- 1:nrow(dat.grow)
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow[cols] <- lapply(dat.grow[cols], factor)

dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(1) = 'HigherManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(2) = 'LowerManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(4) = 'SmallEmployers'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(3,5,6,7,8) = 'Other'"))
dat.grow$W1nssecfam<-relevel(dat.grow$W1nssecfam,ref='HigherManagerial/Professional')

dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(1) = 'FullTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(2) = 'PartTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.grow$W1wrk1aMP<-relevel(dat.grow$W1wrk1aMP,ref="Other")

dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(1) = 'Yes'"))
dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(2) = 'No'"))
dat.grow$W6UnivYP<-relevel(dat.grow$W6UnivYP,ref="No")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(2) = 'OwnMortgage'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1) = 'LivingComfortably'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(2) = 'DoingAlright'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4) = 'QuiteDifficult'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(5) = 'VeryDifficult'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="LivingComfortably")

for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.grow),0.8*nrow(dat.grow) , replace=FALSE)
  training.set<-dat.grow[cross.val,] #the 80% to fit the model
  test.set<-dat.grow[-cross.val,] # the 20% to use as validation sample
  #fit the model
  grow.lm <- grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1wrk1aMP + W1ethgrpYP + W6UnivYP*W8TENURE 
                           + W6OwnchiDV + W8DWRK*W8CMSEX + W8QMAFI + W8DMARSTAT
                           , data = dat.grow)
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(grow.lm,test.set), 
                           #predicted vs original
                           original=test.set$W8GROW,error=(predict(grow.lm,test.set)-test.set$W8GROW))
  
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

# Cross Validation for different splits

dat.grow <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
dat.grow <- dat.grow[, -1]
dat.grow <- subset(dat.grow, W8GROW >= 0 ) 
dat.grow$W8GROW[dat.grow$W8GROW==0]<- 0.05
rownames(dat.grow) <- 1:nrow(dat.grow)
dat.grow[dat.grow <= -1] <- NA
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow[cols] <- lapply(dat.grow[cols], factor)

dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(1) = 'HigherManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(2) = 'LowerManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(4) = 'SmallEmployers'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(3,5,6,7,8) = 'Other'"))
dat.grow$W1nssecfam<-relevel(dat.grow$W1nssecfam,ref='HigherManagerial/Professional')

dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(1) = 'FullTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(2) = 'PartTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.grow$W1wrk1aMP<-relevel(dat.grow$W1wrk1aMP,ref="Other")

dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(1) = 'Yes'"))
dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(2) = 'No'"))
dat.grow$W6UnivYP<-relevel(dat.grow$W6UnivYP,ref="No")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(2) = 'OwnMortgage'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1) = 'LivingComfortably'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(2) = 'DoingAlright'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4) = 'QuiteDifficult'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(5) = 'VeryDifficult'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="LivingComfortably")

split.proportions<-c(0.7,0.8,0.9)
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.grow),split.proportions[i]*nrow(dat.grow) , replace=FALSE)
  training.set<-dat.grow[cross.val,] #the 50/70/90% to fit the model
  test.set<-dat.grow[-cross.val,] # the 50/30/10% to use as validation sample
  #fit the model
  cv.grow.lm<-grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1wrk1aMP + W1ethgrpYP + W6UnivYP*W8TENURE 
                            + W6OwnchiDV + W8DWRK*W8CMSEX + W8QMAFI + W8DMARSTAT
                            , data = dat.grow)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.grow.lm,test.set), 
                           #predicted vs original
                           original=test.set$W8GROW,error=(predict(cv.grow.lm,test.set)-test.set$W8GROW))
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


## Removing rich again but for different splits
dat.grow <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
dat.grow <- dat.grow[, -1]
dat.grow <- subset(dat.grow, W8GROW >= 0 & W8GROW < 2000) 
dat.grow$W8GROW[dat.grow$W8GROW==0]<- 0.05
rownames(dat.grow) <- 1:nrow(dat.grow)
dat.grow[dat.grow <= -1] <- NA
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow[cols] <- lapply(dat.grow[cols], factor)

dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(1) = 'HigherManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(2) = 'LowerManagerial/Professional'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(4) = 'SmallEmployers'"))
dat.grow$W1nssecfam <- with(dat.grow, Recode(W1nssecfam, "c(3,5,6,7,8) = 'Other'"))
dat.grow$W1nssecfam<-relevel(dat.grow$W1nssecfam,ref='HigherManagerial/Professional')

dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(1) = 'FullTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP, "c(2) = 'PartTimePaid'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.grow$W1wrk1aMP <- with(dat.grow, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.grow$W1wrk1aMP<-relevel(dat.grow$W1wrk1aMP,ref="Other")

dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(1) = 'Yes'"))
dat.grow$W6UnivYP <- with(dat.grow, Recode(W6UnivYP, "c(2) = 'No'"))
dat.grow$W6UnivYP<-relevel(dat.grow$W6UnivYP,ref="No")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(2) = 'OwnMortgage'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1) = 'LivingComfortably'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(2) = 'DoingAlright'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4) = 'QuiteDifficult'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(5) = 'VeryDifficult'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="LivingComfortably")

split.proportions<-c(0.7,0.8,0.9)
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.grow),split.proportions[i]*nrow(dat.grow) , replace=FALSE)
  training.set<-dat.grow[cross.val,] #the 50/70/90% to fit the model
  test.set<-dat.grow[-cross.val,] # the 50/30/10% to use as validation sample
  #fit the model
  cv.grow.lm<-grow.lm <- lm(log(W8GROW) ~  W1nssecfam + W1wrk1aMP + W1ethgrpYP + W6UnivYP*W8TENURE 
                            + W6OwnchiDV + W8DWRK*W8CMSEX + W8QMAFI + W8DMARSTAT
                            , data = dat.grow)
  
  #create data frame to use in plots
  pred.val.set<-data.frame(predicted=predict(cv.grow.lm,test.set), 
                           #predicted vs original
                           original=test.set$W8GROW,error=(predict(cv.grow.lm,test.set)-test.set$W8GROW))
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

### NEED TO ADD THE TRANSFORMATIONS, TRYING OUT CENTERING AND OTHER STUFF, FILE NAME FOR 
#read.csv

