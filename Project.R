library(arm)
library(ggplot2)

dat <- read.csv("RWNS_final.csv", header = T)

## Did this after the our intial regression models, had to order the data so the most appropriate baseline was considered
dat$average_k3_score <- rowMeans(dat[, c("k3en", "k3ma", "k3sc")], na.rm = TRUE)


dat$hiquamum <- factor(dat$hiquamum , levels=c("Degree_or_equivalent", "HE_below_degree_level", "GCE_A_Level_or_equivalent"
                                               , "GCSE_grades_A-C_or_equiv", "Other_qualifications", "No_qualification", "missing"))
dat$attitude <- factor(dat$attitude , levels=c("very_high", "high", "low", "very_low"))
dat$FSMband <- factor(dat$FSMband , levels=c("35pr+","21pr-35pr","13pr-21pr", "9pr-13pr", "5pr-9pr","<5pr"))
dat$SECshort <- factor(dat$SECshort , levels=c("Managerial_and_professional", "Intermediate", "Routine,_semi-routine_or_unemployed"))
dat$singlepar <- factor(dat$singlepar , levels=c("no", "yes", "missing"))
dat$house <- factor(dat$house , levels=c("owned", "rented", "other/DK/Ref"))
dat$fsm <- factor(dat$fsm , levels=c("no", "yes", "missing"))
dat$parasp <- factor(dat$parasp , levels=c("No", "Yes", "missing"))
dat$computer <- factor(dat$computer , levels=c("No", "Yes", "missing"))
dat$tuition <- factor(dat$tuition , levels=c("No", "Yes", "missing"))
dat$sen <- factor(dat$sen , levels=c("No", "Yes", "missing"))
dat$absent <- factor(dat$absent , levels=c("No", "Yes", "missing"))
dat$exclude <- factor(dat$exclude , levels=c("No", "Yes", "missing"))


# Added this after doing regressions because wanted to reduce the number of coefficients in the final model
dat$new_hiquamum <- dat$hiquamum
dat$new_homework <- factor(dat$homework)
dat$new_attitude <- dat$attitude
dat$new_FSMband <- dat$FSMband

levels(dat$new_hiquamum)[levels(dat$new_hiquamum)%in%c("Degree_or_equivalent","HE_below_degree_level")] <- "Degree/HE"
levels(dat$new_hiquamum)[levels(dat$new_hiquamum)%in%c("GCE_A_Level_or_equivalent","GCSE_grades_A-C_or_equiv")] <-  "A-Levels/GCSE"

levels(dat$new_homework)[levels(dat$new_homework)%in%c("none","1_evening","2_evenings")] <-  "0-2 evenings"
levels(dat$new_homework)[levels(dat$new_homework)%in%c("3_evenings","4_evenings","5_evenings")] <-  "3-5 evenings"

levels(dat$new_attitude)[levels(dat$new_attitude)%in%c("very_high","high")] <-  "high"
levels(dat$new_attitude)[levels(dat$new_attitude)%in%c("very_low","low")] <- "low"

levels(dat$new_FSMband)[levels(dat$new_FSMband)%in%c("35pr+","21pr-35pr","13pr-21pr")] <-  "13-35+pr"
levels(dat$new_FSMband)[levels(dat$new_FSMband)%in%c("9pr-13pr","5pr-9pr","<5pr")] <-  "<5-13pr"

dat$new_hiquamum <- factor(dat$new_hiquamum , levels=c("Degree/HE", "A-Levels/GCSE", "Other_qualifications", "No_qualification", "missing"))
dat$new_homework <- factor(dat$new_homework , levels=c("0-2 evenings", "3-5 evenings", "missing"))
dat$new_attitude <- factor(dat$new_attitude , levels=c("low", "high", "missing"))
dat$new_FSMband <- factor(dat$new_FSMband , levels=c("<5-13pr","13-35+pr"))

# The same dataset but with the missing values removed
dat.1 <- subset(dat, is.na(IDACI_n)== F & hiquamum != "missing"
                & SECshort != "missing" & singlepar != "missing"
                & fsm != "missing" & parasp != "missing"
                & computer != "missing" & tuition != "missing"
                & homework != "missing" & attitude != "missing"
                & sen != "missing" & truancy != "missing"
                & absent != "missing" & exclude != "missing"
                & FSMband != "NA" & new_hiquamum != "missing" 
                & new_homework != "missing" & new_attitude != "missing" 
                & new_FSMband != "NA" & house != "other/DK/Ref")


head(dat, 4)

summary(dat)

#### EDA ####

# ks4score
p1 <- ggplot(data = dat, aes(y=ks4score)) + geom_boxplot()
p1  

# gender
p1 <- ggplot(data = dat, aes(x=gender, y=ks4score, fill=gender)) + geom_boxplot()
p1  

# hiquamum, significant

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


p1 <- ggplot(data = dat.1, aes(x=new_homework, y=ks4score, fill=new_homework)) + geom_boxplot()
p1  

# attitude
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
p1 <- ggplot(data = dat, aes(x=FSMband, y=ks4score, fill=FSMband)) + geom_boxplot()
p1  

p1 <- ggplot(data = dat.1, aes(x=FSMband, y=ks4score, fill=FSMband)) + geom_boxplot()
p1 

p1 <- ggplot(data = dat.1, aes(x=new_FSMband, y=ks4score, fill=new_FSMband)) + geom_boxplot()
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

# average_k3_score
p1 <- ggplot(data = dat, aes(x=average_k3_score, y=ks4score)) + geom_point()
p1  


cor(dat$k3sc, dat$k3ma)
cor(dat$k3ma, dat$k3en)
cor(dat$k3sc, dat$k3en)

#### LINEAR REGRESSION ####

# Experimenting with a linear model that included all the variables
lm.0 <- lm(ks4score~ ., data=dat)
display(lm.0)
summary(lm.0)
coef(lm.0)

# First model which included all the initial data, except for the repeated columns we added
lm <- lm(ks4score~. -average_k3_score -new_FSMband -new_attitude 
         -new_homework -new_hiquamum, data=dat)
display(lm)
summary(lm)
coef(lm)

par(mfrow=c(2,2))
plot(lm, which=c(1,2))
hist(rstandard(lm), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

# Included all the data but it is a complete case analysis
lm.1 <- lm(ks4score~. -average_k3_score -new_FSMband -new_attitude 
           -new_homework -new_hiquamum, data=dat.1)
display(lm.1, detail = T)
summary(lm.1)
coef(lm.1)

par(mfrow=c(2,2))
plot(lm.1, which=c(1,2))
hist(rstandard(lm.1), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")


# Removed variables we initially thought are not important, based on boxplots and p-values
lm.2 <- lm(ks4score~ . -IDACI_n -tuition -k3en -k3ma -k3sc -new_FSMband -new_attitude 
           -new_homework -new_hiquamum , data=dat)
display(lm.2)
summary(lm.2)
coef(lm.2)

par(mfrow=c(2,2))
plot(lm.2, which=c(1,2))
hist(rstandard(lm.2), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

# Same thing but complete case analysis
lm.3 <- lm(ks4score~ . -IDACI_n -tuition -k3en -k3ma -k3sc -new_FSMband -new_attitude 
           -new_homework -new_hiquamum, data=dat.1)
display(lm.3)
summary(lm.3)
coef(lm.3)

par(mfrow=c(2,2))
plot(lm.3, which=c(1,2))
hist(rstandard(lm.3), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

# Removing some more predictors which are not significant
lm.4 <- lm(ks4score~ . -IDACI_n -SECshort -tuition
           -k3en -k3ma -k3sc -fsm -parasp -absent
           -new_FSMband -new_attitude 
           -new_homework -new_hiquamum, data=dat)
display(lm.4)
summary(lm.4)
coef(lm.4)

par(mfrow=c(2,2))
plot(lm.4, which=c(1,2))
hist(rstandard(lm.4), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

# dat.1
lm.5 <- lm(ks4score~ . -IDACI_n -SECshort -tuition
           -k3en -k3ma -k3sc -fsm -parasp -absent
           -new_FSMband -new_attitude 
           -new_homework -new_hiquamum, data=dat.1)
display(lm.5)
summary(lm.5)
coef(lm.5)

Anova(lm.5)

par(mfrow=c(2,2))
plot(lm.5, which=c(1,2))
hist(rstandard(lm.5), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

lm.6 <- lm(ks4score~ . -IDACI_n -SECshort -tuition 
          -k3en -k3ma -k3sc -fsm -parasp -absent + house*computer
          +new_hiquamum*new_FSMband -hiquamum -attitude -FSMband
          - homework, data=dat.1)
display(lm.6)
summary(lm.6)

coef(lm.6)

par(mfrow=c(2,2))
plot(lm.6, which=c(1,2))
hist(rstandard(lm.6), freq = FALSE ,
     main="Histogram of standardised residuals",
     cex.main=0.8, xlab="Standardised residuals")

Anova(lm.6)


