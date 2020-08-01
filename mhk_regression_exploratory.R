library("tidyverse")
library('ROCR')
library(imbalance)

# fixed.acidity 
# volatile.acidity
# citric.acid
# residual.sugar
# chlorides
# free.sulfur.dioxide
# total.sulfur.dioxide
# density
# pH
# sulphates
# alcohol
# quality
# type


data.white<-read.csv("STAT_Project2/wineQualityWhites.csv", header = TRUE, sep=',')
data.white$type <- 'white'
data.white$q_high <- ifelse(data.white$quality >= 7, 1, 0)

data.red<-read.csv("STAT_Project2/wineQualityReds.csv", header = TRUE, sep=',')
data.red$type <- 'red'
data.red$q_high <- ifelse(data.red$quality >= 7, 1, 0)

#set up which set we want to use
data <- data.white

data.frame(table(data.red$quality))
data.frame(table(data.white$quality))


is.factor(data$q_high)
data$q_high <- factor(data$q_high)
levels(data$q_high) <- c('low','high')
contrasts(data$q_high) 


set.seed(111)
sample<-sample.int(nrow(data), floor(.50*nrow(data)), replace = F)
train<-data[sample, ]
test<-data[-sample, ]

# imbalanceRatio(data.white)
# test_balanced <- oversample(data.white,ratio=.3,c1=NULL, classAttr = 'quality')
# hist(test_balanced$quality)
# hist(test_balanced$quality)
# 
# test_balanced



#Full model with all predictors
model.full <-glm(q_high ~ alcohol + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates , family = "binomial", data=train)
summary(model.full)


##########less citric.acid
model.it_1 <-glm(q_high ~ alcohol + fixed.acidity + volatile.acidity  + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates , family = "binomial", data=train)
summary(model.it_1)


##########less total.sulfur.dioxide
model.it_2 <-glm(q_high ~ alcohol + fixed.acidity + volatile.acidity  + residual.sugar + chlorides + free.sulfur.dioxide + density + pH + sulphates , family = "binomial", data=train)
summary(model.it_2)


##########less alcohol
model.it_3 <-glm(q_high ~ fixed.acidity + volatile.acidity  + residual.sugar + chlorides + free.sulfur.dioxide + density + pH + sulphates , family = "binomial", data=train)
summary(model.it_3)


# test if at least one of the predictors is useful.
delta_g_squared_test_statistic <- model.full$null.deviance-model.full$deviance
df <- 9 # number of predictors
#test if coefficients for all 3 predictors are 0
1-pchisq(delta_g_squared_test_statistic,df)

# We reject the null hypothesis that all the coefficients are zero and can say that
# at least on of the predictors contributes to the statistical significance of the model.

#can I remove some of the predictors?
1-pchisq(model.it_3$null.deviance-model.it_3$deviance,8) 
# small p value means that we reject the null so that at least one of the new predictors
# is not 0. this means that we go with the more complicated model.

#Full vs best
#can I remove some of the predictors?
1-pchisq(model.it_3$deviance-model.full$deviance,3) 
# large p value means that we fail to reject the null that all of the parameters are zero
# This means that we go with the more simpler model.



#ROC 



##predicted survival rate for testing data based on training data
preds<-predict(model.it_3 ,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$q_high)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for High Quality Wines")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-performance(rates, measure = "auc")
auc

##confusion matrix. Actual values in the rows, predicted classification in cols
table(test$q_high, preds>0.5)

table(test$q_high, preds>0.7)
