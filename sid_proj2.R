# Project 2

# Using binary regression
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


full_result <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
summary(full_result)

library(ROCR)
full_preds <- predict(full_result,newdata=test, type="response") # give estimated probability for testing set
full_rates <- prediction(full_preds, test$qualR)
roc_result <- performance(full_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

full_auc <- performance(full_rates, measure = "auc")
full_auc


result <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+residual.sugar+citric.acid+chlorides+free.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
summary(result)


preds <- predict(result,newdata=test, type="response") # give estimated probability for testing set
rates <- prediction(preds, test$qualR)

1-pchisq(result$null.deviance-result$deviance,1)


roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")


auc <- performance(rates, measure = "auc")
auc

table(test$qualR, preds>0.5)
(1789+167)/(1789+167+117+376)

table(test$qualR, preds>0.6)
reduced<-glm(qualR~fixed.acidity+volatile.acidity+residual.sugar+citric.acid+free.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
1-pchisq(reduced$deviance-result$deviance,2)



summary(reduced)


preds1 <- predict(reduced,newdata=test, type="response") # give estimated probability for testing set
rates1 <- prediction(preds1, test$qualR)
roc_result <- performance(rates1,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

auc1 <- performance(rates1, measure = "auc")
auc1

reduced2<-glm(qualR~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
1-pchisq(reduced2$deviance-reduced$deviance,1)

summary(reduced2)


preds2 <- predict(reduced2,newdata=test, type="response") # give estimated probability for testing set
rates2 <- prediction(preds2, test$qualR)
roc_result <- performance(rates2,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

auc2 <- performance(rates2, measure = "auc")
auc2

reduced2<-glm(qualR~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
1-pchisq(reduced2$deviance-reduced$deviance,1)

summary(reduced2)


preds2 <- predict(reduced2,newdata=test, type="response") # give estimated probability for testing set
rates2 <- prediction(preds2, test$qualR)
roc_result <- performance(rates2,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

auc2 <- performance(rates2, measure = "auc")
auc2

table(test$qualR, preds2>0.7)



reduced3<-glm(qualR~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density, family=binomial, data=train)
1-pchisq(reduced3$deviance-reduced2$deviance,2)
1-pchisq(reduced3$deviance-reduced2$deviance,2)
summary(reduced3)



