### Red Wine Detailed Analysis

library(ROCR)

# Read in the red wine data set
dataR <- read.csv("wineQualityReds.csv", header=TRUE, sep=",")

# Establish criteria for "high quality" versus "low quality" red wines
# Ratings of 7 and above (<6) are deemed "high quality"
for (row in 1:nrow(dataR)) {
  if (dataR[row, "quality"] > 6) {
    dataR[row, "qualR"] <- 1
  } else {
    dataR[row, "qualR"] <- 0
  }
}

#Full model before splitting into test and train
full <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates, family=binomial, data=dataR)
summary(full)

# leverages using full model
lev<-lm.influence(full)$hat 
sort(lev)
n <- length(dataR$qualR)
p <- 11
2*p/n # 0.0137586
plot(lev, main="Leverages", ylim=c(-0.1,0.3))
abline(h=2*p/n, col="red")
##identify data points of higher value than the critical value
lev[lev>2*p/n]  # 227 out of 1599 have high leverage

# Using DFFFITs to identify influential observations
DFFITS<-dffits(full)
DFFITS[abs(DFFITS)>2*sqrt(p/n)] #238 out of 1599 are influential


# Split the data set into equal "train" and "test" groups to validate the model
RNGkind(sample.kind = "Rejection")
set.seed(111)

sample <- sample.int(nrow(dataR), floor(.50*nrow(dataR)), replace = F)
train <- dataR[sample,]
test <- dataR[-sample,]

# Inspect the two data sets, verify they both include the new column
str(train)
str(test)

###
### Approach 1 - start with full model, remove predictors that appear insignificant
###

full_result <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
summary(full_result)

full_preds <- predict(full_result,newdata=test, type="response") # Give estimated probability for testing set
full_rates <- prediction(full_preds, test$qualR)
roc_result <- performance(full_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="Red Full Model ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc1 <- performance(full_rates, measure = "auc")
the_auc1@y.values

# Removing Fixed Acidity
red_result <- glm(qualR~alcohol+volatile.acidity+citric.acid+residual.sugar+chlorides+density+free.sulfur.dioxide+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(red_result$deviance-full_result$deviance,1)

summary(red_result)

red_preds <- predict(red_result,newdata=test, type="response") # give estimated probability for testing set
red_rates <- prediction(red_preds, test$qualR)
roc_result <- performance(red_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="Red Reduced Model  ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc2 <- performance(red_rates, measure = "auc")
the_auc2

# Removing Density
next_result <- glm(qualR~alcohol+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(next_result$deviance-red_result$deviance,1)

summary(next_result)

next_preds <- predict(next_result,newdata=test, type="response") # give estimated probability for testing set
next_rates <- prediction(next_preds, test$qualR)
roc_result <- performance(next_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="Red Reduced Model 1 ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc_2 <- performance(next_rates, measure = "auc")
the_auc_2

# Remove free sulfur dioxide
next_result2 <- glm(qualR~alcohol+volatile.acidity+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(next_result2$deviance-next_result$deviance,1)

summary(next_result2) # all predictors have significant p-values, cannot remove any more

next_preds2 <- predict(next_result2,newdata=test, type="response") # give estimated probability for testing set
next_rates2 <- prediction(next_preds2, test$qualR)
roc_result <- performance(next_rates2,measure="tpr", x.measure="fpr")
plot(roc_result, main="Red Reduced Model 2 ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc3 <- performance(next_rates2, measure = "auc")
the_auc3

# Confusion Matrix to validate the model
table(test$qualR, next_preds2>0.5)
#     FALSE TRUE
# 0   669   18
# 1    82   31

# Optimizing threshold to minimize false positive results (FPR) while still maintaining usefulness
table(test$qualR, next_preds2>0.55)
#     FALSE TRUE
# 0   680    7
# 1    90   23

table(test$qualR, next_preds2>0.60) # This reduced false positive by 1, but also reduced true positive by 7.
#     FALSE TRUE
# 0   681    6
# 1    97   16

###
### Approach 2 - start with observations from Exploratory Data Analysis
###

next_result4 <- glm(qualR~alcohol+volatile.acidity+residual.sugar+density+sulphates, family=binomial, data=train)
1-pchisq(next_result4$null.deviance-next_result4$deviance,5) # Result of 0 means the model is useful
summary(next_result4) # Remove density due to insignificant p-value and high standard error

next_preds4 <- predict(next_result4,newdata=test, type="response") # give estimated probability for testing set
next_rates4 <- prediction(next_preds4, test$qualR)
roc_result <- performance(next_rates4,measure="tpr", x.measure="fpr")
plot(roc_result, main="New Red ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc5 <- performance(next_rates4, measure = "auc")
the_auc5@y.values


table(test$qualR, next_preds4>0.5)


table(test$qualR, next_preds4>0.55)

# Remove density due to insignificant p-value and high standard error

next_result5 <- glm(qualR~alcohol+volatile.acidity+residual.sugar+sulphates, family=binomial, data=train)
1-pchisq(next_result5$deviance-next_result4$deviance,1) # Result of 0 means the model is useful
summary(next_result5) # Remove density due to insignificant p-value and high standard error

next_preds5 <- predict(next_result5,newdata=test, type="response") # give estimated probability for testing set
next_rates5 <- prediction(next_preds5, test$qualR)
roc_result <- performance(next_rates5,measure="tpr", x.measure="fpr")
plot(roc_result, main="New Reduced Red ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc6 <- performance(next_rates5, measure = "auc")
the_auc6@y.values # 0.88645

# Confusion Matrix to validate the model and compare versus Approach 1 above
table(test$qualR, next_preds5>0.5)
#     FALSE TRUE
# 0   682    5
# 1    92   21
# Sensitivity = 1-FNR = TP/(FN+TP)
# Sensitivity = 21/(21+92) = 0.1858407
# 18.6% of high quality wines are classified correctly
# Specificity = 1-FPR = TN/(TN+FP)
# Specificity = # 682/(682+5) = 0.992722
# 99.3% of low quality wines are classified correctly
# Accuracy = 1-Error Rate = 1-((92+5)/(92+5+682+21)) = 0.87875
# Accuracy = 87.9 % 

# Optimizing threshold to minimize false positive results (FPR) while still maintaining usefulness
table(test$qualR, next_preds5>0.55) # reduced FPR by 3, TPR by 4
#     FALSE TRUE
# 0   685    2
# 1    96   17
# Sensitivity = 1-FNR = TP/(FN+TP)
# Sensitivity = 17/(17+96) = 0.1504425
# 15.0% of high quality wines are classified correctly
# Specificity = 1-FPR = TN/(TN+FP)
# Specificity = # 685/(685+2) = 0.9970888 ** this is most important for our customer
# 99.7% of low quality wines are classified correctly
# Accuracy = 1-Error Rate = 1-((96+2)/(96+2+685+17)) = 0.8775
# Accuracy = 87.7 % 


# Revisiting the exploratory data analysis
# These last models still were not better than the previous best: new_result5
next_result6 <- glm(qualR~alcohol+volatile.acidity+citric.acid+residual.sugar+sulphates, family=binomial, data=train)
summary(next_result6) 
next_preds6 <- predict(next_result6,newdata=test, type="response") # give estimated probability for testing set
next_rates6 <- prediction(next_preds6, test$qualR)
roc_result <- performance(next_rates6,measure="tpr", x.measure="fpr")
plot(roc_result, main="New Reduced Red ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc7 <- performance(next_rates6, measure = "auc")
the_auc7@y.values # 0.88645
table(test$qualR, next_preds6>0.55)

next_result7 <- glm(qualR~alcohol+volatile.acidity+citric.acid+sulphates, family=binomial, data=train)
summary(next_result7) 
next_preds7 <- predict(next_result7,newdata=test, type="response") # give estimated probability for testing set
next_rates7 <- prediction(next_preds7, test$qualR)
roc_result <- performance(next_rates7,measure="tpr", x.measure="fpr")
plot(roc_result, main="New Reduced Red ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc8 <- performance(next_rates7, measure = "auc")
the_auc8@y.values 
table(test$qualR, next_preds7>0.55)

next_result8 <- glm(qualR~alcohol+volatile.acidity+sulphates, family=binomial, data=train)
summary(next_result8) 
next_preds8 <- predict(next_result8,newdata=test, type="response") # give estimated probability for testing set
next_rates8 <- prediction(next_preds8, test$qualR)
roc_result <- performance(next_rates8,measure="tpr", x.measure="fpr")
plot(roc_result, main="New Reduced Red ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc9 <- performance(next_rates8, measure = "auc")
the_auc9@y.values
table(test$qualR, next_preds7>0.55)




