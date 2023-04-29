library(arm)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(hablar)
library(readxl)
library(caret)
library(car)
library(corrplot)

dat <- read.csv("EOTST2112023_GROW.csv", header = T)
data.type <- read_xlsx("DataDictTranslation.xlsx")

colname.vector <- colnames(dat)

num.columns <- unlist(filter(data.type, Type == "continuous")[,4, drop=FALSE])
num.columns <- intersect(num.columns, colname.vector)
factor.columns <- unlist(filter(data.type, Type != "continuous")[,4, drop=FALSE])
factor.columns <- intersect(factor.columns, colname.vector)

num.columns
factor.columns

dat <- dat %>%
  convert(num(num.columns), fct(factor.columns))

str(dat)

dat <- subset(dat, select = -c(NSID))

lm.0 <- lm(W8GROW~ ., data=dat)
display(lm.0)
summary(lm.0)
        
cor(dat)

dat_centered <- dat %>%
  mutate_if(is.numeric, function(x) (x - mean(x))/sd(x))
summary(dat_centered)

lm.1 <- lm(W8GROW~ ., data=dat_centered)        
summary_lm.1 <- summary(lm.1)
alias_coef <- rownames(summary_lm.1$coefficients)[is.na(summary_lm.1$coefficients)]
if (length(alias_coef) > 0) {
  predictor_variables <- predictor_variables[, !colnames(predictor_variables) %in% alias_coef]
  lm.2 <- lm(response_variable ~ predictor_variables, data = dataset)
}

summary(lm.2)
