### Vino da 'Ville
### Michael Kolonay (mhk9c)
### Jordan Machita (jm8ux)
### Stephen Morris (sam3ce)
### Siddharth Surapeneni (sss2e)

### White Wine Detailed Analysis

library(ggplot2)
library(ROCR)
library(tidyverse)  

# Using binary regression

setwd("STAT_Project2")
dataW <- read.csv("wineQualityWhites.csv", header=TRUE, sep=",")
dataR <- read.csv("wineQualityReds.csv", header=TRUE, sep=",")

for (row in 1:nrow(dataW)) {
  if (dataW[row, "quality"] > 6) {
    dataW[row, "qualR"] <- 1
  } else {
    dataW[row, "qualR"] <- 0
  }
}

dataW

RNGkind(sample.kind = "Rejection")
set.seed(111)

sample <- sample.int(nrow(dataW), floor(.50*nrow(dataW)), replace = F)
train <- dataW[sample,]
test <- dataW[-sample,]

train
is.factor(dataW$qualR)
dataW$qualR <- factor(dataW$qualR)


###
### Approach 1 - start with full model, remove predictors that appear insignificant
###

full_result <- glm(qualR~alcohol+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
summary(full_result)

#candidate predictors to remove:
#                      Estimate   Std. Error  z value Pr(>|z|) 
#alcohol               1.014e-02  1.641e-01   0.062  0.95075  
#citric.acid          -3.327e-01  5.836e-01  -0.570  0.56858 
#free.sulfur.dioxide   4.553e-03  4.224e-03   1.078  0.28111
#total.sulfur.dioxide  1.709e-03  2.155e-03   0.793  0.42765

#Check model
full_preds <- predict(full_result,newdata=test, type="response") # Give estimated probability for testing set
full_rates <- prediction(full_preds, test$qualR)
roc_result <- performance(full_rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="White Full Model ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc1 <- performance(full_rates, measure = "auc")
auc_value <- the_auc1@y.values[1] #0.7939588


confusion <- table(test$qualR, full_preds>0.5)

#     FALSE TRUE
# 0   1819   98
# 1   388  144

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.2706767 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9488785 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.8015517 #
wine_optim <- TP/(FP+TP) #0.5827338





##########################
# 1 remove alcohol
##########################

result <- glm(qualR~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
summary(result)

#candidate predictors to remove:
#                      Estimate   Std. Error  z value Pr(>|z|) 
#citric.acid          -3.307e-01  5.826e-01  -0.568  0.57031 
#free.sulfur.dioxide   4.513e-03  4.176e-03   1.081  0.27979 
#total.sulfur.dioxide  1.734e-03  2.118e-03   0.819  0.41291

#Check model
preds <- predict(result,newdata=test, type="response") # Give estimated probability for testing set
rates <- prediction(preds, test$qualR)
roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="White Reduced Model ROC 1")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc <- performance(rates, measure = "auc")
the_auc@y.values #0.7939153


confusion <- table(test$qualR, preds>0.5)

#     FALSE TRUE
# 0   1789  117
# 1   381  162

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.2983425 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9386149 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.7966517 #
wine_optim <- TP/(FP+TP) #0.5806452


##########################
# 2 remove citric.acid
##########################

result <- glm(qualR~fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
summary(result)

#candidate predictors to remove:
#                      Estimate   Std. Error  z value Pr(>|z|) 
#free.sulfur.dioxide   4.644e-03  4.170e-03   1.114  0.26547
#total.sulfur.dioxide  1.569e-03  2.100e-03   0.747  0.45498   

#Check model
preds <- predict(result,newdata=test, type="response") # Give estimated probability for testing set
rates <- prediction(preds, test$qualR)
roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="White Reduced Model ROC 2")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc <- performance(rates, measure = "auc")
the_auc@y.values #0.7937192

confusion <- table(test$qualR, preds>0.5)

#     FALSE TRUE
# 0   1790  116
# 1   376  167

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.3075506 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9391396 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.7991017 #
wine_optim <- TP/(FP+TP) #0.590106


##########################
# 3 remove total.sulfur.dioxide
##########################

result <- glm(qualR~fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+density+pH+sulphates, family=binomial, data=train)
summary(result)


#Check model
preds <- predict(result,newdata=test, type="response") # Give estimated probability for testing set
rates <- prediction(preds, test$qualR)
roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="White Reduced Model ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc <- performance(rates, measure = "auc")
the_auc@y.values #0.7943926


# At this point all the predictors are significant. Test the model with the confusion matrix.
# Criteria is to minimize False positives - AKA to not recommend wines as high quality when they are in fact not.
# Optimizing threshold to minimize false positive results (FPR) while still maintaining usefulness

##########################
#threshold .5
#########################
confusion <- table(test$qualR, preds>0.5)

#     FALSE TRUE
# 0   1789  117
# 1   376  167

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.3075506 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9386149 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.7986933 #
wine_optim <- TP/(FP+TP) #0.5880282

##########################
#threshold .55
#########################
confusion <- table(test$qualR, preds>0.55)

#     FALSE TRUE
# 0   1833   73
# 1   422  121

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.2228361 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9616999 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.7978767 #
wine_optim <- TP/(FP+TP) #0.6237113

##########################
#threshold .60
#########################
confusion <- table(test$qualR, preds>0.60)

#     FALSE TRUE
# 0   1863   43
# 1   451   92

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.1694291 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9774397 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.798285 #
wine_optim <- TP/(FP+TP) #0.6814815


##########################
#threshold .65
#########################
confusion <- table(test$qualR, preds>0.65)

#     FALSE TRUE
# 0   1881   25
# 1   487   56

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.1031308 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9868835 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.7909351 #
wine_optim <- TP/(FP+TP) #0.691358

##########################
#threshold .7
#########################
confusion <- table(test$qualR, preds>0.7)

#     FALSE TRUE
# 0   1895   11
# 1   517   26

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.04788214 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.7844018 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.7909351 #
wine_optim <- TP/(FP+TP) #0.7027027










###
### Approach 2 - start with observations from Exploratory Data Analysis
###


# exploratory analysis predictors: alcohol, citric acid, residual sugar, chlorides, total sulfur dioxide, density

##########################
# 1 Predictors based on EDA
##########################

# round 1: alcohol, citric acid, residual sugar, chlorides, total sulfur dioxide, density (full model based on EDA)
result <- glm(qualR~alcohol+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+density, family=binomial, data=train)
summary(result)

#candidate predictors to remove:
#                      Estimate   Std. Error  z value Pr(>|z|) 
#citric.acid           2.326e-01  5.153e-01   0.451  0.65179 
#total.sulfur.dioxide  1.533e-03  1.595e-03   0.961  0.33652  

#Check model
preds <- predict(result,newdata=test, type="response") # Give estimated probability for testing set
rates <- prediction(preds, test$qualR)
roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="White EDA Model ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc <- performance(rates, measure = "auc")
the_auc@y.values #0.7572742

confusion <- table(test$qualR, preds>0.5)

#     FALSE TRUE
# 0   1804  102
# 1   391  152

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.2799263 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9464848 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.7986933 #
wine_optim <- TP/(FP+TP) #0.5984252


##########################
# 2 Predictors based on EDA : drop citric acid
##########################

# using backward elimination on our full model from EDA and checking confusion matrix: drop citric acid
result <- glm(qualR~alcohol+residual.sugar+chlorides+total.sulfur.dioxide+density, family=binomial, data=train)
summary(result)

#candidate predictors to remove:
#                      Estimate   Std. Error  z value Pr(>|z|) 
#total.sulfur.dioxide  1.590e-03  1.588e-03   1.001 0.316839    
#density              -1.410e+02  7.227e+01  -1.951 0.051036 .

#Check model
preds <- predict(result,newdata=test, type="response") # Give estimated probability for testing set
rates <- prediction(preds, test$qualR)
roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="White EDA Model ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc <- performance(rates, measure = "auc")
the_auc@y.values #0.7573061


confusion <- table(test$qualR, preds>0.5)

#     FALSE TRUE
# 0   1805  101
# 1   390  153

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.281768 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9470094 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.79951 #
wine_optim <- TP/(FP+TP) #0.6023622

##########################
# 3 Predictors based on EDA : drop sulfur dioxide
##########################

# using backward elimination and checking confusion matrix: drop total sulfur dioxide
result <- glm(qualR~alcohol+residual.sugar+chlorides+density, family=binomial, data=train)
summary(result)

#candidate predictors to remove:
#                      Estimate   Std. Error  z value Pr(>|z|) 
#density              -122.94871   69.73427  -1.763 0.077883 .

#Check model
preds <- predict(result,newdata=test, type="response") # Give estimated probability for testing set
rates <- prediction(preds, test$qualR)
roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="White EDA Model ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc <- performance(rates, measure = "auc")
the_auc@y.values #0.7581912

##########################
#threshold .5
#########################
confusion <- table(test$qualR, preds>0.5)

#     FALSE TRUE
# 0   1806  100
# 1   390  153

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.281768 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9475341 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.7999183 #
wine_optim <- TP/(FP+TP) #0.6047431


# 100 false pos, 153 true pos at 0.5
# this is the best in terms of confusion matrix
# but use 0.55 over 0.5 (trying to maximize TP/(FP+TP) without the numbers getting too small)
# judgment call on above for what cutoff maximizes without getting too small, can be higher than 0.55


##########################
#threshold .55
#########################
confusion <- table(test$qualR, preds>0.55)

#     FALSE TRUE
# 0   1848   58
# 1   431  112

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.2062615 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9695698 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.8003267 #
wine_optim <- TP/(FP+TP) #0.6588235




##########################
# 4 Predictors based on EDA : drop  density
##########################

# using backward elimination and checking confusion matrix: drop density
result <- glm(qualR~alcohol+residual.sugar+chlorides, family=binomial, data=train)
summary(result)

# all predictors are now significant

#Check model
preds <- predict(result,newdata=test, type="response") # Give estimated probability for testing set
rates <- prediction(preds, test$qualR)
roc_result <- performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="White EDA Model ROC")
lines(x = c(0,1), y = c(0,1), col="red")

the_auc <- performance(rates, measure = "auc")
the_auc@y.values #0.7576863


##########################
#threshold .5
#########################
confusion <- table(test$qualR, preds>0.5)

#     FALSE TRUE
# 0   1802  104
# 1   382  161

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.2965009 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9454355 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.8015517 #
wine_optim <- TP/(FP+TP) #0.6075472

##########################
#threshold .55
#########################
confusion <- table(test$qualR, preds>0.55)

#     FALSE TRUE
# 0   1847   59
# 1   441  102

TN <- confusion[1]
FN <- confusion[2]
FP <- confusion[3]
TP <- confusion[4]
Sensitivity <- TP/(FN+TP)#0.1878453 # % of high quality wines are classified correctly
Specificity <- TN/(TN+FP)#0.9690451 # % of low quality wines are classified correctly
Accuracy <- 1 - (FN + FP)/(FN+FP+TN+TP) #0.795835 #
wine_optim <- TP/(FP+TP) #0.6335404


# all predictors are now significant
# 104 false pos, 161 true pos at 0.5 (slightly worse when you look at a percentage basis)
# thus, we should include density in the model


