dataR <- read.csv("wineQualityReds.csv", header=TRUE, sep=",")

for (row in 1:nrow(dataR)) {
  if (dataR[row, "quality"] > 6) {
    dataR[row, "qualR"] <- 1
  } else {
    dataR[row, "qualR"] <- 0
  }
}

set.seed(111)

sample <- sample.int(nrow(dataR), floor(.50*nrow(dataR)), replace = F)
train <- dataR[sample,]
test <- dataR[-sample,]

train
is.factor(dataR$qualR)
dataR$qualR <- factor(dataR$qualR)


full_result <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
summary(full_result)

library(ROCR)
full_preds <- predict(full_result,newdata=test, type="response") # give estimated probability for testing set
full_rates <- prediction(full_preds, test$qualR)
roc_result <- performance(full_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc <- performance(full_rates, measure = "auc")
the_auc

#Removing Density,Sulfates,Fixed Acidity
next_result <- glm(qualR~alcohol+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(next_result$deviance-full_result$deviance,2)

summary(next_result)

library(ROCR)
next_preds <- predict(next_result,newdata=test, type="response") # give estimated probability for testing set
next_rates <- prediction(next_preds, test$qualR)
roc_result <- performance(next_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc2 <- performance(next_rates, measure = "auc")
the_auc2

#Drop free sulfur dioxide
next_result2 <- glm(qualR~alcohol+volatile.acidity+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(next_result2$deviance-next_result$deviance,1)

summary(next_result2)

library(ROCR)
next_preds2 <- predict(next_result2,newdata=test, type="response") # give estimated probability for testing set
next_rates2 <- prediction(next_preds2, test$qualR)
roc_result <- performance(next_rates2,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc3 <- performance(next_rates2, measure = "auc")
the_auc3

#Drop residual.sugar
next_result3 <- glm(qualR~alcohol+volatile.acidity+citric.acid+chlorides+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(next_result3$deviance-next_result2$deviance,1)

summary(next_result3)

library(ROCR)
next_preds3 <- predict(next_result3,newdata=test, type="response") # give estimated probability for testing set
next_rates3 <- prediction(next_preds3, test$qualR)
roc_result <- performance(next_rates3,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc3 <- performance(next_rates3, measure = "auc")
the_auc3

table(test$qualR, next_preds3>0.5)







