# Project 2 - Stat 6021

setwd("~/Desktop/Stat 6021/Project 2")
dataW <- read.csv("wineQualityWhites.csv", header=TRUE, sep=",")
dataR <- read.csv("wineQualityReds.csv", header=TRUE, sep=",")


#cor(data)
#pairs(data, lower.panel=NULL)
# high correlation predictors: residual sugar and density (0.84); 
  # free sulfur dioxide and total sulfur dioxide (0.62)
  # alcohol and density (-0.78)

# what has high correlation with quality?
  # alcohol is highest at 0.44 then density at -0.31 then chlorides at -0.21

hist(quality) # mostly 6s, followed by 5 then 7, a few 8s and 4s, very few 9s and 3s
hist(alcohol) # generally in the 8.5-10.5 range and then right tail heading toward 14
hist(pH) # all within one pH mostly around 3.2
hist(density) # pretty much all 0.99 to 1.00 weird?
hist(sulphates) # whites are mostly 0.3 to 0.6, reds are 0.5 to 0.8 so a bit more sulphates
hist(total.sulfur.dioxide) # 100-200 range, much higher than reds
hist(chlorides) # range from .02 to .07 ish, slightly lower than reds (0.05 to .1)
hist(citric.acid) # pretty similar to reds, most like 0.25 to 0.5
hist(volatile.acidity)# very heavily 0.1 to 0.4; reds: pretty even from 0.3 to 0.8 (more volatile acidity)
hist(fixed.acidity) # both mostly 6-9 but reds have larger right tail

plot(total.sulfur.dioxide,free.sulfur.dioxide) # pretty linear, definitely don't need both in the model

plot(alcohol, density)


?hist




# multinomial logistic regression

library(nnet)

for (row in 1:nrow(dataW)) {
  if (dataW[row, "quality"] == 9) {
    dataW[row, "quality"] <- 8 
  } 
}

dataW

is.numeric(dataW$quality)
dataW$quality <- factor(dataW$quality)
levels(dataW$quality)

set.seed(199)
sample <- sample.int(nrow(dataW), floor(.50*nrow(dataW)), replace = F)
train <- dataW[sample, ] # takes the sample
test <- dataW[-sample, ] # takes those that weren't in the sample

train


result <- multinom(data=train, quality ~ alcohol + density + sulphates + pH + fixed.acidity + 
                     volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide
                   + total.sulfur.dioxide)
summary(result)

z <- summary(result)$coefficients/summary(result)$standard.errors
z

p <- (1 - pnorm(abs(z)))*2
p


result2 <- multinom(data=train, quality ~ alcohol + density + sulphates + pH + fixed.acidity + 
                     volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide)
summary(result2)

z <- summary(result2)$coefficients/summary(result2)$standard.errors
z

p <- (1 - pnorm(abs(z)))*2
p


result3 <- multinom(data=train, quality ~ alcohol + density + sulphates + fixed.acidity + 
                      volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide)
summary(result3)

z <- summary(result3)$coefficients/summary(result3)$standard.errors
z

p <- (1 - pnorm(abs(z)))*2
p


result4 <- multinom(data=train, quality ~ alcohol + density + sulphates + fixed.acidity + 
                      volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide)
summary(result4)

z <- summary(result4)$coefficients/summary(result4)$standard.errors
z

p <- (1 - pnorm(abs(z)))*2
p


result5 <- multinom(data=train, quality ~ alcohol + density + fixed.acidity + 
                      volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide)
summary(result5)

z <- summary(result5)$coefficients/summary(result5)$standard.errors
z

p <- (1 - pnorm(abs(z)))*2
p


reduced <- multinom(data=train, quality ~ alcohol + density + fixed.acidity + 
                      volatile.acidity + chlorides + free.sulfur.dioxide)
summary(reduced)

z <- summary(reduced)$coefficients/summary(reduced)$standard.errors
z

p <- (1 - pnorm(abs(z)))*2
p


# Use deviance tests:

# full model is stored as result

# Our reduced model vs. intercept-only model:
1-pchisq(reduced$null.deviance-reduced$deviance,7)
# equal to zero, reject null and use reduced model over intercept only

# Our reduced model vs. full model
1-pchisq(reduced$deviance-result$deviance,6)
# very tiny, less than 0.05 so we use the reduced model over the full

summary(reduced)




