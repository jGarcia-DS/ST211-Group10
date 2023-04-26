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
dat.grow <- subset(dat.grow, W8GROW > 0) 
rownames(dat.grow) <- 1:nrow(dat.grow)
dat.grow[dat.grow <= -1] <- NA

summary(dat.grow)

#### GROW
## Needed to be below 1000 to have a good look at all the boxplots
dat.grow.norich<-subset(dat.grow,W8GROW<=1000)

# cont data boxplots
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


#checking the 30% level


# Calculate the proportion of continuous containing more than 30%
proportion <- sum(dat.grow$W1GrssyrMP < 0) / length(dat.grow$W1GrssyrMP)
proportion

proportion <- sum(dat.grow$W1yschat1 < 0) / length(dat.grow$W1yschat1)
proportion

proportion <- sum(dat.grow$W2ghq12scr < 0) / length(dat.grow$W2ghq12scr)
proportion

proportion <- sum(dat.grow$W6DebtattYP < 0) / length(dat.grow$W6DebtattYP)
proportion

proportion <- sum(dat.grow$W8DGHQSC < 0) / length(dat.grow$W8DGHQSC)
proportion



# Converting columns to categorical predictors using factor
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow[cols] <- lapply(dat.grow[cols], factor)


## Linear models of predictors that we removed
grow.lm <- lm(W8GROW ~ W1GrssyrMP, data = dat.grow)
display(grow.lm)
summary(grow.lm)


# make a liner model without grassy
grow.lm <- lm(W8GROW ~ W1yschat1 + W1wrk1aMP + W1condur5MP + W1hea2MP + W1NoldBroHS
              + W1InCarHH + W1hous12HH + W1usevcHH  + W1hiqualdad + W1wrkfulldad
              + W1wrkfullmum + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH
              + W1IndSchool + W1marstatmum + W1nssecfam  + W1ethgrpYP + W1heposs9YP
              + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP  + W6acqno
              + W6gcse + W6OwnchiDV + W6DebtattYP 
              + W8DDEGP + W8DGHQSC + W8DMARSTAT + W8DWRK
              + W8CMSEX + W8TENURE + W8QMAFI
              
              , data = dat.grow)

vif(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


## log transform
grow.lm <- lm(log(W8GROW) ~ W1yschat1 + W1wrk1aMP + W1condur5MP + W1hea2MP + W1NoldBroHS
              + W1InCarHH + W1hous12HH + W1usevcHH  + W1hiqualdad + W1wrkfulldad
              + W1wrkfullmum + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH
              + W1IndSchool + W1marstatmum + W1nssecfam  + W1ethgrpYP + W1heposs9YP
              + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP  + W6acqno
              + W6gcse + W6OwnchiDV + W6DebtattYP 
              + W8DDEGP + W8DGHQSC + W8DMARSTAT + W8DWRK
              + W8CMSEX + W8TENURE + W8QMAFI
              , data = dat.grow)
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



# for W6acqno W6gcse W6als



summary(lm(W8GROW ~ W6acqno    
           , data = dat.grow))

summary(lm(W8GROW ~ W6gcse   
           , data = dat.grow))

summary(lm(W8GROW ~ W6als   
           , data = dat.grow))


# for W8DWRK and W8DACTIVITYC

summary(lm(W8GROW ~ W8DWRK   
           , data = dat.grow))

summary(lm(W8GROW ~ W8DACTIVITYC   
           , data = dat.grow))




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
dat.grow[outliers,]

outliers <- Reduce(intersect,list(grow.lm.stats$Leverage, grow.lm.stats$Std.res))
outliers.2 <- outliers
dat.grow[outliers,]


outliers <- Reduce(intersect,list(grow.lm.stats$Leverage, grow.lm.stats$DFFITS))
outliers.3 <- outliers
dat.grow[outliers,]

all.outliers <- union(union(outliers.1, outliers.2), outliers.3)
dat.grow[all.outliers,]


## outliers 
no.outlier.dat <-dat.grow[-all.outliers,]



no.outlier.dat.lm <- lm(log(W8GROW) ~ W1yschat1 + W1wrk1aMP + W1condur5MP + W1hea2MP + W1NoldBroHS
                        + W1InCarHH + W1hous12HH + W1usevcHH  + W1hiqualdad + W1wrkfulldad
                        + W1wrkfullmum + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH
                        + W1IndSchool + W1marstatmum + W1nssecfam  + W1ethgrpYP + W1heposs9YP
                        + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
                        + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP  + W6acqno
                        + W6gcse + W6OwnchiDV + W6DebtattYP 
                        + W8DDEGP + W8DGHQSC + W8DMARSTAT + W8DWRK
                        + W8CMSEX + W8TENURE + W8QMAFI , data = no.outlier.dat)
vif(no.outlier.dat.lm)

display(no.outlier.dat.lm)
summary(no.outlier.dat.lm)


par(mfrow=c(2,2))

plot(no.outlier.dat.lm, which=c(1,2))
hist(rstandard(no.outlier.dat.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


# No extreme values
no.extreme.dat <- subset(dat.grow, 1 < W8GROW & W8GROW < 3000) 
#no.extreme.dat <- no.extreme.value.dat[-all.outliers,]
no.extreme.lm <- lm(log(W8GROW) ~ W1yschat1 + W1wrk1aMP + W1condur5MP + W1hea2MP + W1NoldBroHS
                        + W1InCarHH + W1hous12HH + W1usevcHH  + W1hiqualdad + W1wrkfulldad
                        + W1wrkfullmum + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH
                        + W1IndSchool + W1marstatmum + W1nssecfam  + W1ethgrpYP + W1heposs9YP
                        + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
                        + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP  + W6acqno
                        + W6gcse + W6OwnchiDV + W6DebtattYP 
                        + W8DDEGP + W8DGHQSC + W8DMARSTAT + W8DWRK
                        + W8CMSEX + W8TENURE + W8QMAFI , data = no.extreme.dat)

display(no.extreme.lm)
summary(no.extreme.lm)

par(mfrow=c(2,2))

plot(no.extreme.lm, which=c(1,2))
hist(rstandard(no.extreme.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")



# Proceeded with original dataset
grow.lm <- lm(log(W8GROW) ~ W1yschat1 + W1wrk1aMP + W1condur5MP + W1hea2MP + W1NoldBroHS
              + W1InCarHH + W1hous12HH + W1usevcHH  + W1hiqualdad + W1wrkfulldad
              + W1wrkfullmum + W1ch0_2HH + W1ch3_11HH + W1ch12_15HH + W1ch16_17HH
              + W1IndSchool + W1marstatmum + W1nssecfam  + W1ethgrpYP + W1heposs9YP
              + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP  + W6acqno
              + W6gcse + W6OwnchiDV + W6DebtattYP 
              + W8DDEGP + W8DGHQSC + W8DMARSTAT + W8DWRK
              + W8CMSEX + W8TENURE + W8QMAFI
              , data = dat.grow)
summary(grow.lm)

Anova(grow.lm)


## Removing the non-significant predictors at the 5% level
grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP + W1heposs9YP + W6UnivYP + W6acqno +W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(grow.lm)



### Merging some levels of categorical predictors
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno, "c(1,2) = 'DegreeHE'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno, "c(3,4) = 'A-levels'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno,"c(5,6) = 'GCSE'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno,"c(7,8,9) = 'Other'"))
dat.grow$W6acqno<-relevel(dat.grow$W6acqno,ref="DegreeHE")

dat.grow$W1heposs9YP <- with(dat.grow, Recode(W1heposs9YP, "c(1,2) = 'Likely'"))
dat.grow$W1heposs9YP <- with(dat.grow, Recode(W1heposs9YP, "c(3,4) = 'NotLikely'"))
dat.grow$W1heposs9YP<-relevel(dat.grow$W1heposs9YP,ref="Likely")

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,2,3) = 'Own'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1,2) = 'AboveAverage'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'Average'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4,5) = 'BelowAverage'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="AboveAverage")

grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP + W1heposs9YP + W6UnivYP + W6acqno +W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(grow.lm)


# Significant
grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno +W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

# Not significant
grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP*W6UnivYP + W6acqno +W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

# Not significant
grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno + W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8TENURE*W8QMAFI
              , data = dat.grow)

summary(grow.lm)

# Not significant
grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno 
              + W8DMARSTAT + W8DWRK*W6OwnchiDV + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

# Not significant
grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno
              + W8DMARSTAT*W6OwnchiDV + W8DWRK + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

# Not significant
grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP*W6acqno
              + W8DMARSTAT + W6OwnchiDV + W8DWRK + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

# Significant
grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno
              + W8DMARSTAT + W6OwnchiDV + W8TENURE + W8QMAFI*W8DWRK
              , data = dat.grow)

summary(grow.lm)

# Significant
grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno
              + W8DMARSTAT*W8CMSEX + W6OwnchiDV + W8TENURE + W8QMAFI*W8DWRK
              , data = dat.grow)

summary(grow.lm)

# FINAL MODEL

grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno
              + W8DMARSTAT*W8CMSEX + W6OwnchiDV + W8TENURE + W8QMAFI*W8DWRK
              , data = dat.grow)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

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

grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP + W1heposs9YP + W6UnivYP + W6acqno +W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(grow.lm)

dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno, "c(1,2) = 'DegreeHE'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno, "c(3,4) = 'A-levels'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno,"c(5,6) = 'GCSE'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno,"c(7,8,9) = 'Other'"))
dat.grow$W6acqno<-relevel(dat.grow$W6acqno,ref="DegreeHE")

dat.grow$W1heposs9YP <- with(dat.grow, Recode(W1heposs9YP, "c(1,2) = 'Likely'"))
dat.grow$W1heposs9YP <- with(dat.grow, Recode(W1heposs9YP, "c(3,4) = 'NotLikely'"))
dat.grow$W1heposs9YP<-relevel(dat.grow$W1heposs9YP,ref="Likely")

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,2,3) = 'Own'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1,2) = 'AboveAverage'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'Average'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4,5) = 'BelowAverage'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="AboveAverage")



grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno
              + W8DMARSTAT*W8CMSEX + W6OwnchiDV + W8TENURE + W8QMAFI*W8DWRK
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

grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP + W1heposs9YP + W6UnivYP + W6acqno +W6OwnchiDV 
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE + W8QMAFI
              , data = dat.grow)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(grow.lm)

dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.grow$W1ethgrpYP <- with(dat.grow, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.grow$W1ethgrpYP<-relevel(dat.grow$W1ethgrpYP,ref="White")

dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno, "c(1,2) = 'DegreeHE'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno, "c(3,4) = 'A-levels'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno,"c(5,6) = 'GCSE'"))
dat.grow$W6acqno <- with(dat.grow, Recode(W6acqno,"c(7,8,9) = 'Other'"))
dat.grow$W6acqno<-relevel(dat.grow$W6acqno,ref="DegreeHE")

dat.grow$W1heposs9YP <- with(dat.grow, Recode(W1heposs9YP, "c(1,2) = 'Likely'"))
dat.grow$W1heposs9YP <- with(dat.grow, Recode(W1heposs9YP, "c(3,4) = 'NotLikely'"))
dat.grow$W1heposs9YP<-relevel(dat.grow$W1heposs9YP,ref="Likely")

dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.grow$W6OwnchiDV <- with(dat.grow, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.grow$W6OwnchiDV<-relevel(dat.grow$W6OwnchiDV,ref="No")

dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.grow$W8DMARSTAT <- with(dat.grow, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.grow$W8DMARSTAT<-relevel(dat.grow$W8DMARSTAT,ref="Single")

dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(1) = 'Employed'"))
dat.grow$W8DWRK <- with(dat.grow, Recode(W8DWRK, "c(2) = 'NotEmployed'"))
dat.grow$W8DWRK<-relevel(dat.grow$W8DWRK,ref="Employed")

dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.grow$W8CMSEX <- with(dat.grow, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.grow$W8CMSEX<-relevel(dat.grow$W8CMSEX,ref="Male")

dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(1,2,3) = 'Own'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(4) = 'Rent'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.grow$W8TENURE <- with(dat.grow, Recode(W8TENURE, "c(7) = 'Other'"))
dat.grow$W8TENURE<-relevel(dat.grow$W8TENURE,ref="Rent")

dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(1,2) = 'AboveAverage'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(3) = 'Average'"))
dat.grow$W8QMAFI <- with(dat.grow, Recode(W8QMAFI, "c(4,5) = 'BelowAverage'"))
dat.grow$W8QMAFI<-relevel(dat.grow$W8QMAFI,ref="AboveAverage")



grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno
              + W8DMARSTAT*W8CMSEX + W6OwnchiDV + W8TENURE + W8QMAFI*W8DWRK
              , data = dat.grow)

summary(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(grow.lm)


# Cross Validation

for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.grow),0.8*nrow(dat.grow) , replace=FALSE)
  training.set<-dat.grow[cross.val,] #the 80% to fit the model
  test.set<-dat.grow[-cross.val,] # the 20% to use as validation sample
  #fit the model
  grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno
                + W8DMARSTAT*W8CMSEX + W6OwnchiDV + W8TENURE + W8QMAFI*W8DWRK
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
split.proportions<-c(0.7,0.8,0.9)
for(i in 1:3){
  #create training/test sets
  cross.val<-sample(1:nrow(dat.grow),split.proportions[i]*nrow(dat.grow) , replace=FALSE)
  training.set<-dat.grow[cross.val,] #the 50/70/90% to fit the model
  test.set<-dat.grow[-cross.val,] # the 50/30/10% to use as validation sample
  #fit the model
  cv.grow.lm<-grow.lm <- lm(log(W8GROW) ~ W1ethgrpYP*W8CMSEX + W1heposs9YP + W6UnivYP + W6acqno
                          + W8DMARSTAT*W8CMSEX + W6OwnchiDV + W8TENURE + W8QMAFI*W8DWRK
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




