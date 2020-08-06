### Vino da 'Ville
### Michael Kolonay (mhk9c)
### Jordan Machita (jm8ux)
### Stephen Morris (sam3ce)
### Siddharth Surapeneni (sss2e)

### Red vs White Wine Analysis

setwd("~/Desktop/Stat 6021/Project 2")
dataW <- read.csv("wineQualityWhites.csv", header=TRUE, sep=",")
dataR <- read.csv("wineQualityReds.csv", header=TRUE, sep=",")


?density

# COMPARISON OF DENSITY FUNCTIONS FOR RED VERSUS WHITES

# alcohol overlapping density plots
d <- density(dataW$alcohol)
d2 <- density(dataR$alcohol)
plot(d2, col="red")
lines(d, col="black", add=T)

# quality overlapping density plots
d <- density(dataW$quality)
d2 <- density(dataR$quality)
plot(d, col="black")
lines(d2, col="red", add=T)

# pH overlapping density plots
d <- density(dataW$pH)
d2 <- density(dataR$pH)
plot(d, col="black")
lines(d2, col="red", add=T)

# sulphates overlapping density plots
d <- density(dataW$sulphates)
d2 <- density(dataR$sulphates)
plot(d, col="black")
lines(d2, col="red", add=T)

# density overlapping density plots
d <- density(dataW$density)
d2 <- density(dataR$density)
plot(d2, col="red")
lines(d, col="black", add=T)

# total.sulfur.dioxide overlapping density plots
d <- density(dataW$total.sulfur.dioxide)
d2 <- density(dataR$total.sulfur.dioxide)
plot(d2, col="red")
lines(d, col="black", add=T)

# free.sulfur.dioxide overlapping density plots
d <- density(dataW$free.sulfur.dioxide)
d2 <- density(dataR$free.sulfur.dioxide)
plot(d2, col="red")
lines(d, col="black", add=T)

# chlorides overlapping density plots
d <- density(dataW$chlorides)
d2 <- density(dataR$chlorides)
plot(d, col="black")
lines(d2, col="red", add=T)

# citric.acid overlapping density plots
d <- geom_density(dataW$citric.acid)
d2 <- geom_density(dataR$citric.acid)
plot(d, col="black")
lines(d2, col="red", add=T)

# volatile.acidity overlapping density plots
d <- geom_density(dataW$volatile.acidity)
d2 <- geom_density(dataR$volatile.acidity)
plot(d, col="black")
lines(d2, col="red", add=T)

# fixed.acidity overlapping density plots
d <- geom_density(dataW$fixed.acidity)
d2 <- geom_density(dataR$fixed.acidity)
plot(d, col="black")
lines(d2, col="red", add=T)



?plot


cor(dataW)
#pairs(data, lower.panel=NULL)
# high correlation predictors: residual sugar and density (0.84); 
  # free sulfur dioxide and total sulfur dioxide (0.62)
  # alcohol and density (-0.78)

# what has high correlation with quality?
  # alcohol is highest at 0.44 then density at -0.31 then chlorides at -0.21

# histograms

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

boxplot(alcohol~quality) # yes?
boxplot(pH~quality) # nope
boxplot(density~quality) # 


plot(total.sulfur.dioxide,free.sulfur.dioxide) # pretty linear, definitely don't need both in the model

plot(alcohol, density)


?hist


### IGNORE BELOW: MULTINOMIAL ###

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
get_prediction<-function

# test_set<-predict(reduced,test,"probs")

get_prediction<-function(row){
  index=1;
  max=row[index];
  for(i in 1:length(row)){
    if(row[i]>max){
      index=i;
      max=row[i];}
  }
  result=3
  if(index == 2){
    result=4
  }else if(index==3){
    result=5
  }else if(index==4){
    result=6
  }else if(index==5){
    result=7
  }else if(index==6){
    result=8
  }else{
    result=3
  }
  result
}
get_prediction(test_set[2,])
print(test_set[2,])

prediction_results<-apply(test_set,1,get_prediction)



