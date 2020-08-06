### Vino da 'Ville
### Michael Kolonay (mhk9c)
### Jordan Machita (jm8ux)
### Stephen Morris (sam3ce)
### Siddharth Surapeneni (sss2e)

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
1-pchisq(full_result$null.deviance-full_result$deviance,11)

full_preds <- predict(full_result,newdata=test, type="response") # Give estimated probability for testing set
full_rates <- prediction(full_preds, test$qualR)
roc_result <- performance(full_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="Red Full Model ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc1 <- performance(full_rates, measure = "auc")
the_auc1@y.values #0.89027

table(test$qualR, full_preds>0.5)
#    FALSE TRUE
#0   672   15
#1    76   37
#Precision: 35/(35+17)=0.673
#Specificity= 672/(672+15)=0.978
#Sensitivity= 37/(37+76)=0.327
#Accuracy=(672+37)/(672+15+76+37)=0.88625

# Removed pH
red_result <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+density+free.sulfur.dioxide+total.sulfur.dioxide+sulphates, family=binomial, data=train)
#red_result <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+density+free.sulfur.dioxide+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(red_result$deviance-full_result$deviance,1) #p-value of .95

summary(red_result)

red_preds <- predict(red_result,newdata=test, type="response") # give estimated probability for testing set
red_rates <- prediction(red_preds, test$qualR)
roc_result <- performance(red_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="Red Reduced Model  ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc2 <- performance(red_rates, measure = "auc")
the_auc2@y.values #0.89026

table(test$qualR, red_preds>0.5)
#    FALSE TRUE
#0   672   15
#1    77   36
#Precision: 36/(36+15)=0.7058
#Specificity= 672/(672+15)=0.978
#Sensitivity= 37/(37+76)=0.3186
#Accuracy=(672+37)/(672+15+77+36)=0.885

# Removing Free sulfur dioxide
next_result <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+density+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(next_result$deviance-red_result$deviance,1)

summary(next_result)

next_preds <- predict(next_result,newdata=test, type="response") # give estimated probability for testing set
next_rates <- prediction(next_preds, test$qualR)
roc_result <- performance(next_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="Red Reduced Model 1 ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc_2 <- performance(next_rates, measure = "auc")
the_auc_2@y.values #0.8900826

table(test$qualR, next_preds>0.5)
#    FALSE TRUE
#0   672   15
#1    77   36
#Precision: 36/(36+15)=0.7058
#Specificity= 672/(672+15)=0.978
#Sensitivity= 37/(37+76)=0.3186
#Accuracy=(672+37)/(672+15+77+36)=0.885

# Removed citric acid
next_result2 <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+residual.sugar+chlorides+density+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(next_result2$deviance-next_result$deviance,1)

summary(next_result2) # all predictors have significant p-values, cannot remove any more

next_preds2 <- predict(next_result2,newdata=test, type="response") # give estimated probability for testing set
next_rates2 <- prediction(next_preds2, test$qualR)
roc_result <- performance(next_rates2,measure="tpr", x.measure="fpr")
plot(roc_result, main="Red Reduced Model 2 ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc3 <- performance(next_rates2, measure = "auc")
the_auc3@y.values #.8914
table(test$qualR, next_preds2>0.5)
#    FALSE TRUE
#0   673   14
#1    77   36
#Precision: 36/(36+14)=0.720
#Specificity= 673/(673+14)=0.9796
#Sensitivity= 36/(36+77)=0.3186
#Accuracy=(673+36)/(673+14+77+36)=0.88625

# Removed density
next_result3 <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+residual.sugar+chlorides+total.sulfur.dioxide+sulphates, family=binomial, data=train)
1-pchisq(next_result3$deviance-next_result2$deviance,1)

summary(next_result3) # all predictors have significant p-values, cannot remove any more

next_preds3 <- predict(next_result3,newdata=test, type="response") # give estimated probability for testing set
next_rates3 <- prediction(next_preds3, test$qualR)
roc_result <- performance(next_rates3,measure="tpr", x.measure="fpr")
plot(roc_result, main="Red Reduced Model 3 ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc4 <- performance(next_rates3, measure = "auc")
the_auc4@y.values #0.8895

# Confusion Matrix to validate the model
table(test$qualR, next_preds3>0.45)
#     FALSE TRUE
#0   662   25
#1    75   38

# Optimizing threshold to minimize false positive results (FPR) while still maintaining usefulness
table(test$qualR, next_preds3>0.50)
   
#    FALSE TRUE
#  0   669   18
#  1    79   34

table(test$qualR, next_preds3>0.55) 
#   FALSE TRUE
#0   678    9
#1    87   26

###
### Approach 2 - start with observations from Exploratory Data Analysis
###

next_result4 <- glm(qualR~alcohol+volatile.acidity+citric.acid+density+sulphates, family=binomial, data=train)
1-pchisq(next_result4$null.deviance-next_result4$deviance,5) # Result of 0 means the model is useful
summary(next_result4) # Remove density due to insignificant p-value and high standard error

next_preds4 <- predict(next_result4,newdata=test, type="response") # give estimated probability for testing set
next_rates4 <- prediction(next_preds4, test$qualR)
roc_result <- performance(next_rates4,measure="tpr", x.measure="fpr")
plot(roc_result, main="EDA Initial Red ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc5 <- performance(next_rates4, measure = "auc")
the_auc5@y.values #0.8839768

table(test$qualR, next_preds4>0.55)
#    FALSE TRUE
#0   682    5
#1    95   18


table(test$qualR, next_preds4>0.50)
#    FALSE TRUE
#0   678    9
#1    91   22
# Sensitivity = 1-FNR = TP/(FN+TP)
# Sensitivity = 22/(22+91) = 0.1946
# 18.6% of high quality wines are classified correctly
# Specificity = 1-FPR = TN/(TN+FP)
# Specificity = # 678/(678+9) = 0.9869
# 98.69% of low quality wines are classified correctly
# Accuracy = 1-Error Rate = 1-((91+9)/(91+9+678+22)) = 0.875
# Accuracy = 87.5 % 

# Remove density due to insignificant p-value and high standard error

next_result5 <- glm(qualR~alcohol+volatile.acidity+citric.acid+sulphates, family=binomial, data=train)
1-pchisq(next_result5$deviance-next_result4$deviance,1) # Result of 0.48 means density should be removed
summary(next_result5) # Remove density due to insignificant p-value and high standard error

next_preds5 <- predict(next_result5,newdata=test, type="response") # give estimated probability for testing set
next_rates5 <- prediction(next_preds5, test$qualR)
roc_result <- performance(next_rates5,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC for EDA model")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc6 <- performance(next_rates5, measure = "auc")
the_auc6@y.values # 0.8840541

# Confusion Matrix to validate the model and compare versus Approach 1 above
table(test$qualR, next_preds5>0.5)
#     FALSE TRUE
# 0   679    8
# 1    90   23
# Sensitivity = 1-FNR = TP/(FN+TP)
# Sensitivity = 23/(23+90) = 0.2035
# 20.35% of high quality wines are classified correctly
# Specificity = 1-FPR = TN/(TN+FP)
# Specificity = # 679/(679+8) = 0.9884
# 98.84% of low quality wines are classified correctly
# Accuracy = 1-Error Rate = 1-((90+8)/(90+8+679+23)) = 0.8775
# Accuracy = 87.75 % 

#  FALSE TRUE
#0   683    4
#1    95   18
table(test$qualR, next_preds5>0.55)

#   FALSE TRUE
#0   683    4
#1    95   18

table(test$qualR, next_preds5>0.45)

#    FALSE TRUE
#0   670   17
#1    86   27

#MODEL with Residual Sugars
next_result6 <- glm(qualR~alcohol+volatile.acidity+residual.sugar+density+sulphates, family=binomial, data=train)
1-pchisq(next_result6$null.deviance-next_result6$deviance,5) # Result of 0 means the model is useful
summary(next_result6) # Remove density due to insignificant p-value and high standard error

next_preds6 <- predict(next_result6,newdata=test, type="response") # give estimated probability for testing set
next_rates6 <- prediction(next_preds6, test$qualR)
roc_result <- performance(next_rates6,measure="tpr", x.measure="fpr")
plot(roc_result, main="Initial EDA Adjusted Red ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc6 <- performance(next_rates6, measure = "auc")
the_auc6@y.values


table(test$qualR, next_preds6>0.5)
#With density removed
next_result7 <- glm(qualR~alcohol+volatile.acidity+residual.sugar+sulphates, family=binomial, data=train)
1-pchisq(next_result7$deviance-next_result6$deviance,1) 
summary(next_result7) 

next_preds7 <- predict(next_result7,newdata=test, type="response") # give estimated probability for testing set
next_rates7 <- prediction(next_preds7, test$qualR)
roc_result <- performance(next_rates7,measure="tpr", x.measure="fpr")
plot(roc_result, main=" Red Wine ROC for EDA with Residual Sugars")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc7 <- performance(next_rates7, measure = "auc")
the_auc7@y.values # 0.88645

# Confusion Matrix to validate the model and compare versus Approach 1  and above
table(test$qualR, next_preds7>0.5)

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
table(test$qualR, next_preds7>0.55)
#   FALSE TRUE
#0   685   2
#1    96   17
# Sensitivity = 1-FNR = TP/(FN+TP)
# Sensitivity = 17/(17+96) = 0.1504
# 18.6% of high quality wines are classified correctly
# Specificity = 1-FPR = TN/(TN+FP)
# Specificity = # 685/(685+2) = 0.997
# 99.7% of low quality wines are classified correctly
# Accuracy = 1-Error Rate = 1-((92+5)/(92+5+682+21)) = 0.8775
# Accuracy = 87.625 
table(test$qualR, next_preds7>0.45)
#   FALSE TRUE
#0   673   14
#1    85   28
# Sensitivity = 1-FNR = TP/(FN+TP)
# Sensitivity = 28/(28+85) = 0.2477
# 18.6% of high quality wines are classified correctly
# Specificity = 1-FPR = TN/(TN+FP)
# Specificity = # 673/(673+14) = 0.9796
# 97.96% of low quality wines are classified correctly
# Accuracy = 1-Error Rate = 1-((85+14)/(28+85+673+14) = 0.87625
# Accuracy = 85.875 % 

next_result8 <- glm(qualR~alcohol+volatile.acidity+sulphates, family=binomial, data=train)
1-pchisq(next_result8$deviance-next_result7$deviance,1) 
summary(next_result8) 

next_preds8 <- predict(next_result8,newdata=test, type="response") # give estimated probability for testing set
next_rates8 <- prediction(next_preds8, test$qualR)
roc_result <- performance(next_rates8,measure="tpr", x.measure="fpr")
plot(roc_result, main=" Final EDA Ajusted Red ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc8 <- performance(next_rates8, measure = "auc")
the_auc8@y.values # 0.884051
table(test$qualR, next_preds8>0.5)
table(test$qualR, next_preds8>0.45)
table(test$qualR, next_preds8>0.55)
