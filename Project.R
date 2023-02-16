library(arm)
library(ggplot2)

dat <- read.csv("RWNS_final.csv", header = T)

dat$average_score <- rowMeans(dat[, c("k3en", "k3ma", "k3sc")], na.rm = TRUE)

# Missing could be an indicator, so think about what you what to do
dat.1 <- subset(dat, is.na(IDACI_n)== F & hiquamum != "missing"
                & SECshort != "missing" & singlepar != "missing"
                & fsm != "missing" & parasp != "missing"
                & computer != "missing" & tuition != "missing"
                & homework != "missing" & attitude != "missing"
                & sen != "missing" & truancy != "missing"
                & absent != "missing" & exclude != "missing"
                & FSMband != "NA")


head(dat, 4)

summary(dat)

# gender, non significant
p1 <- ggplot(data = dat, aes(x=gender, y=ks4score, fill=gender)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=gender, y=ks4score, fill=gender)) + geom_boxplot()
p1  

# hiquamum, significant
dat$hiquamum <- factor(dat$hiquamum , levels=c("Degree_or_equivalent", "HE_below_degree_level", "GCE_A_Level_or_equivalent"
                                               , "GCSE_grades_A-C_or_equiv", "Other_qualifications", "No_qualification", "missing"))
p1 <- ggplot(data = dat, aes(x=hiquamum, y=ks4score, fill=hiquamum)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=hiquamum, y=ks4score, fill=hiquamum)) + geom_boxplot()
p1  

# SECshort, not that significant
p1 <- ggplot(data = dat, aes(x=SECshort, y=ks4score, fill=SECshort)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=SECshort, y=ks4score, fill=SECshort)) + geom_boxplot()
p1  

# singepar, not that significant
p1 <- ggplot(data = dat, aes(x=singlepar, y=ks4score, fill=singlepar)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=singlepar, y=ks4score, fill=singlepar)) + geom_boxplot()
p1  

# house, not that significant
p1 <- ggplot(data = dat, aes(x=house, y=ks4score, fill=house)) + geom_boxplot()
p1  

# fsm, not that significant
p1 <- ggplot(data = dat, aes(x=fsm, y=ks4score, fill=fsm)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=fsm, y=ks4score, fill=fsm)) + geom_boxplot()
p1  

# parasp, significant
p1 <- ggplot(data = dat, aes(x=parasp, y=ks4score, fill=parasp)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=parasp, y=ks4score, fill=parasp)) + geom_boxplot()
p1  

# computer, significant
p1 <- ggplot(data = dat, aes(x=computer, y=ks4score, fill=computer)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=computer, y=ks4score, fill=computer)) + geom_boxplot()
p1  

# tuition, not significant
p1 <- ggplot(data = dat, aes(x=tuition, y=ks4score, fill=tuition)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=tuition, y=ks4score, fill=tuition)) + geom_boxplot()
p1 

# pupasp, significant
p1 <- ggplot(data = dat,aes(x=pupasp, y=ks4score, fill=pupasp)) + geom_boxplot()
p1  

# hw
p1 <- ggplot(data = dat, aes(x=homework, y=ks4score, fill=homework)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=homework, y=ks4score, fill=homework)) + geom_boxplot()
p1  

# attitude
dat$attitude <- factor(dat$attitude , levels=c("very_low", "low", "high", "very_high"))
p1 <- ggplot(data = dat, aes(x=attitude, y=ks4score, fill=attitude)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=attitude, y=ks4score, fill=attitude)) + geom_boxplot()
p1  

# sen
p1 <- ggplot(data = dat, aes(x=sen, y=ks4score, fill=sen)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=sen, y=ks4score, fill=sen)) + geom_boxplot()
p1  

# truancy
p1 <- ggplot(data = dat, aes(x=truancy, y=ks4score, fill=truancy)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=truancy, y=ks4score, fill=truancy)) + geom_boxplot()
p1  

# absent
p1 <- ggplot(data = dat, aes(x=absent, y=ks4score, fill=absent)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=absent, y=ks4score, fill=absent)) + geom_boxplot()
p1  

# exclude
p1 <- ggplot(data = dat, aes(x=exclude, y=ks4score, fill=exclude)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=exclude, y=ks4score, fill=exclude)) + geom_boxplot()
p1  

# IDACI_n
p1 <- ggplot(data = dat, aes(x=IDACI_n, y=ks4score, fill=IDACI_n)) + geom_point()
p1  

p1 <- ggplot(data = dat.1, aes(x=IDACI_n, y=ks4score, fill=IDACI_n)) + geom_point()
p1 

# FSMband
dat$FSMband <- factor(dat$FSMband , levels=c("<5pr", "5pr-9pr", "9pr-13pr", "13pr-21pr","21pr-35pr","35pr+"))
p1 <- ggplot(data = dat, aes(x=FSMband, y=ks4score, fill=FSMband)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=FSMband, y=ks4score, fill=FSMband)) + geom_boxplot()
p1 

# fiveac
p1 <- ggplot(data = dat, aes(x=fiveac, y=ks4score, fill=fiveac)) + geom_boxplot()
p1  

# fiveem
p1 <- ggplot(data = dat,aes(x=fiveem, y=ks4score, fill=fiveem)) + geom_boxplot()
p1  

# k3en
p1 <- ggplot(data = dat, aes(x=k3en, y=ks4score, fill=factor(k3en))) + geom_boxplot()
p1  

# k3ma
p1 <- ggplot(data = dat, aes(x=k3ma, y=ks4score, fill=factor(k3ma))) + geom_boxplot()
p1  

# k3sc
p1 <- ggplot(data = dat, aes(x=k3sc, y=ks4score, fill=factor(k3sc))) + geom_boxplot()
p1  

p1 <- ggplot(data = dat, aes(x=average_score, y=ks4score, fill=factor(average_score))) + geom_boxplot()
p1  


lm <- lm(ks4score~ ., data=dat)
display(lm)
summary(lm)
coef(lm)

par(mfrow=c(2,2))
plot(lm, which=c(1,2))
hist(rstandard(lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

lm.1 <- lm(ks4score~ ., data=dat.1)
display(lm.1, detail = T)
summary(lm.1)
coef(lm.1)

par(mfrow=c(2,2))
plot(lm.1, which=c(1,2))
hist(rstandard(lm.1), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


# Removed variables we initially thought are not important
lm.2 <- lm(ks4score~ . -IDACI_n -gender -SECshort -tuition -fiveem
           -k3en -k3ma -k3sc, data=dat)
display(lm.2)
summary(lm.2)
coef(lm.2)

par(mfrow=c(2,2))
plot(lm.2, which=c(1,2))
hist(rstandard(lm.2), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

lm.3 <- lm(ks4score~ . -IDACI_n -gender -SECshort -tuition -fiveem
           -k3en -k3ma -k3sc, data=dat.1)
display(lm.3)
summary(lm.3)
coef(lm.3)

par(mfrow=c(2,2))
plot(lm.3, which=c(1,2))
hist(rstandard(lm.3), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

# second model
lm.4 <- lm(ks4score~ . -IDACI_n -gender -SECshort -tuition -fiveem
           -k3en -k3ma -k3sc -fsm -singlepar -parasp -computer
           -absent, data=dat)
display(lm.4)
summary(lm.4)
coef(lm.4)

par(mfrow=c(2,2))
plot(lm.4, which=c(1,2))
hist(rstandard(lm.4), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


# dropping more variables we found to be not significant
lm.5 <- lm(ks4score~ . -IDACI_n -gender -SECshort -tuition -fiveem
           -k3en -k3ma -k3sc -fsm -singlepar -parasp -computer
           -absent, data=dat.1)
display(lm.5)
summary(lm.5)
coef(lm.5)

par(mfrow=c(2,2))
plot(lm.5, which=c(1,2))
hist(rstandard(lm.5), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


