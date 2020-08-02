# Project 2

# Using binary regression

setwd("~/Desktop/Stat 6021/Project 2")
dataW <- read.csv("wineQualityWhites.csv", header=TRUE, sep=",")
dataR <- read.csv("wineQualityReds.csv", header=TRUE, sep=",")

for (row in 1:nrow(dataW)) {
  if (dataW[row, "quality"] > 6) {
    dataW[row, "qualR"] <- 1
  } else {
    dataW[row, "qualR"] <- 0
  }
}

# box plots of binary quality rating for EDA
dataW
boxplot(dataW$alcohol~dataW$qualR) # looks useful
boxplot(dataW$fixed.acidity~dataW$qualR) # definitely not
boxplot(dataW$volatile.acidity~dataW$qualR) # probs not
boxplot(dataW$citric.acid~dataW$qualR) # maybe?
boxplot(dataW$residual.sugar~dataW$qualR)# possibly
boxplot(dataW$chlorides~dataW$qualR) # probably
boxplot(dataW$free.sulfur.dioxide~dataW$qualR) # nope
boxplot(dataW$total.sulfur.dioxide~dataW$qualR) # maybe
boxplot(dataW$density~dataW$qualR) # maybe
boxplot(dataW$pH~dataW$qualR) # nope
boxplot(dataW$sulphates~dataW$qualR) # nope


set.seed(111)

sample <- sample.int(nrow(dataW), floor(.50*nrow(dataW)), replace = F)
train <- dataW[sample,]
test <- dataW[-sample,]

train
is.factor(dataW$qualR)
dataW$qualR <- factor(dataW$qualR)

# backward elimination from full model:
result <- glm(qualR~fixed.acidity+volatile.acidity+residual.sugar+chlorides+density+sulphates, family=binomial, data=train)
summary(result)

# exploratory analysis predictors:

# round 1: alcohol, citric acid, residual sugar, chlorides, total sulfur dioxide, density (full model based on EDA)
result <- glm(qualR~alcohol+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+density, family=binomial, data=train)
summary(result)

# using backward elimination on our full model from EDA and checking confusion matrix: drop citric acid
result <- glm(qualR~alcohol+residual.sugar+chlorides+total.sulfur.dioxide+density, family=binomial, data=train)
summary(result)

# using backward elimination and checking confusion matrix: drop total sulfur dioxide
result <- glm(qualR~alcohol+residual.sugar+chlorides+density, family=binomial, data=train)
summary(result)
# 100 false pos, 153 true pos at 0.5
# this is the best in terms of confusion matrix
# but use 0.55 over 0.5 (trying to maximize TP/(FP+TP) without the numbers getting too small)
# judgement call on above for what cutoff maximizes without getting too small, can be higher than 0.55

# using backward elimination and checking confusion matrix: drop density
result <- glm(qualR~alcohol+residual.sugar+chlorides, family=binomial, data=train)
summary(result)
# all predictors are now significant
# 104 false pos, 161 true pos at 0.5 (slightly worse when you look at a percentage basis)
# thus, we should include density in the model



library(ROCR)
preds <- predict(result,newdata=test, type="response") # give estimated probability for testing set
rates <- prediction(preds, test$qualR)
roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

auc <- performance(rates, measure = "auc")
auc@y.values

table(test$qualR, preds>0.5)

table(test$qualR, preds>0.55)
