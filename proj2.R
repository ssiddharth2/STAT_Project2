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

set.seed(111)

sample <- sample.int(nrow(dataW), floor(.50*nrow(dataW)), replace = F)
train <- dataW[sample,]
test <- dataW[-sample,]

train
is.factor(dataW$qualR)
dataW$qualR <- factor(dataW$qualR)

result <- glm(qualR~fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
summary(result)

library(ROCR)
preds <- predict(result,newdata=test, type="response") # give estimated probability for testing set
rates <- prediction(preds, test$qualR)
roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

auc <- performance(rates, measure = "auc")
auc

table(test$qualR, preds>0.5)
(1789+167)/(1789+167+117+376)

table(test$qualR, preds>0.6)
