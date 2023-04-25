library(arm)
library(ggplot2)
library(car)
library(tidyr)

options(max.print=9999999) # So we can increase the amount that is printed in R


dat.grow <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
# Removing NSID
dat.grow <- dat.grow[, -1]
# Making sure that the outcome is fully observed
dat.grow <- subset(dat.grow, W8GROW > 0) 
#dat.grow[dat.grow < 0] <- NA

summary(dat.grow)

#### GROW
## Needed to be below 1000 to have a good look at all the boxplots
dat.grow.norich<-subset(dat.grow,W8GROW<=1000)


special.dat <- gather(data = dat.grow.norich[, c(1, 2, 29, 40, 42, 49)], -W8GROW, key = "var", value = "value")
p1 <- ggplot(special.dat, aes(x = value, y = W8GROW)) + geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~var, scales = "free_x")
p1 <- p1 + theme_bw() + scale_fill_grey() + theme(legend.position = "none")
p1


special.dat <- gather(data = dat.grow.norich[, -c(1, 2, 29, 40, 42)], -W8GROW, key = "var", value = "value")
p2 <- ggplot(special.dat, aes(x = factor(value), y = W8GROW)) + geom_point(size = 0.5) +
  geom_boxplot() + facet_wrap(~var, scales = "free_x")

p2 <- p2 + theme_bw() + scale_fill_grey() + theme(legend.position = "none")
p2


#############################################################
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

#############################################################


# Converting columns to categorical predictors using factor
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow[cols] <- lapply(dat.grow[cols], factor)

grow.lm <- lm(W8GROW ~ W1GrssyrMP, data = dat.grow)
display(grow.lm)
summary(grow.lm)

vif(grow.lm)

par(mfrow=c(2,2))
plot(grow.lm, which=c(1,2))

hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


# centering
dat.grow.copy <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
dat.grow.copy$cent.W1yschat1 <- with(dat.grow.copy, W1yschat1 - mean(W1yschat1))
dat.grow.copy$cent.W2ghq12scr <- with(dat.grow.copy, W2ghq12scr - mean(W2ghq12scr))
dat.grow.copy$cent.W6DebtattYP <- with(dat.grow.copy, W6DebtattYP - mean(W6DebtattYP))
dat.grow.copy$cent.W8DGHQSC <- with(dat.grow.copy, W8DGHQSC - mean(W8DGHQSC))
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow.copy[cols] <- lapply(dat.grow.copy[cols], factor)

grow.lm <- lm(W8GROW ~.   -NSID -W1GrssyrMP
                          -W1yschat1 
                          -W2ghq12scr
                          -W6DebtattYP 
                          -W8DGHQSC, data = dat.grow.copy)
display(grow.lm)
summary(grow.lm)


# Standerdising 
dat.grow.copy <- read.csv("EOTST2112023_GROW.csv", header = TRUE)
dat.grow.copy$std.W1yschat1 <- with(dat.grow.copy, (W1yschat1 - mean(W1yschat1))/sd(W1yschat1))
dat.grow.copy$std.W2ghq12scr <- with(dat.grow.copy, (W2ghq12scr - mean(W2ghq12scr))/sd(W2ghq12scr))
dat.grow.copy$std.W6DebtattYP <- with(dat.grow.copy, (W6DebtattYP - mean(W6DebtattYP))/sd(W6DebtattYP))
dat.grow.copy$std.W8DGHQSC <- with(dat.grow.copy, (W8DGHQSC - mean(W8DGHQSC))/sd(W8DGHQSC))

cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,
          32,33,34,35,36,37,38,39,41,43,44,45,46,47,48)
dat.grow.copy[cols] <- lapply(dat.grow.copy[cols], factor)

grow.lm <- lm(W8GROW ~.   -NSID  -W1GrssyrMP
              -W1yschat1 
              -W2ghq12scr
              -W6DebtattYP 
              -W8DGHQSC, data = dat.grow.copy)
display(grow.lm)
summary(grow.lm)


# Log transform on the outcome

grow.lm <- lm(log(W8GROW) ~ . -W1GrssyrMP -W1condur5MP  -W1hea2MP  -W1NoldBroHS -W1InCarHH -W1usevcHH -W1hiqualdad
              -W1wrkfulldad  -W1wrkfullmum -W1empsdad -W1ch0_2HH -W1ch3_11HH  -W1ch12_15HH
              -W1ch16_17HH  -W1marstatmum -W1famtyp2 -W1nssecfam -W1hwndayYP 
              -W1truantYP -W1alceverYP -W1bulrc -W2depressYP -W6EducYP -W6Apprent1YP -W8DWRK
              -W8DMARSTAT, data = dat.grow)
vif(grow.lm)

display(grow.lm)
summary(grow.lm)

par(mfrow=c(2,2))

plot(grow.lm, which=c(1,2))
hist(rstandard(grow.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


# -W1condur5MP -W1hea2MP -W1NoldBroHS -W1InCarHH -W1usevcHH -W1hiqualdad -W1wrkfulldad -W1wrkfullmum
# -W1empsdad -W1ch0_2HH -W1ch3_11HH -W1ch12_15HH -W1ch16_17HH -W1marstatmum  -W1famtyp2 -W1nssecfam
# -W1hwndayYP -W1truantYP -W1alceverYP -W1bulrc -W2depressYP -W6EducYP -W6Apprent1YP -W8DWRK



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
  list.of.stats <- list(Std.res = res.out, Std.res.top = res.top, Leverage = lev.out,
                        Leverage.top = lev.top, DFFITS = dffits.out, DFFITS.top = dffits.top,
                        Cooks = cooks.out, Cooks.top = cooks.top) # return the statistics
  list.of.stats
}


grow.lm.stats<-show_outliers(grow.lm, 5)
grow.lm.stats


outliers <- Reduce(intersect,list(grow.lm.stats$Leverage, grow.lm.stats$DFFITS))
outliers
dat.grow[outliers,]

## outliers 
no.outlier.dat <-dat.grow[-outliers,]
nrow(no.outlier.dat)
nrow(dat.grow)



no.outlier.dat.lm <- lm(log(W8GROW) ~ . -W1condur5MP  -W1hea2MP  -W1NoldBroHS -W1InCarHH -W1usevcHH -W1hiqualdad
              -W1wrkfulldad  -W1wrkfullmum -W1empsdad -W1ch0_2HH -W1ch3_11HH  -W1ch12_15HH
              -W1ch16_17HH  -W1marstatmum -W1famtyp2 -W1nssecfam -W1hwndayYP 
              -W1truantYP -W1alceverYP -W1bulrc -W2depressYP -W6EducYP -W6Apprent1YP -W8DWRK
              -W8DMARSTAT, data = no.outlier.dat)
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
#no.extreme.dat <- no.extreme.value.dat[-outliers,]
no.extreme.dat.lm <- lm(log(W8GROW) ~ . -W1condur5MP  -W1hea2MP  -W1NoldBroHS -W1InCarHH -W1usevcHH -W1hiqualdad
                        -W1wrkfulldad  -W1wrkfullmum -W1empsdad -W1ch0_2HH -W1ch3_11HH  -W1ch12_15HH
                        -W1ch16_17HH  -W1marstatmum -W1famtyp2 -W1nssecfam -W1hwndayYP 
                        -W1truantYP -W1alceverYP -W1bulrc -W2depressYP -W6EducYP -W6Apprent1YP -W8DWRK
                        -W8DMARSTAT, data = no.extreme.dat)

display(no.extreme.dat.lm)
summary(no.extreme.dat.lm)

par(mfrow=c(2,2))

plot(no.extreme.dat.lm, which=c(1,2))
hist(rstandard(no.extreme.dat.lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")



