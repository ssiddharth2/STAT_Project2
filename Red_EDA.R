### Portuguese Wine Analysis
### Team "Vinho da C'Ville"
### Michael Kolonay, Jordan Machida, Stephen Morris, Siddharth Surapaneni

library(MASS)
library(RColorBrewer)
library(ggplot2)
library(nnet)

red<-read.csv("wineQualityReds.csv", header=TRUE)
white<-read.csv("wineQualityWhites.csv", header=TRUE)
attach(red)

### Exploratory Data Analysis

boxplot(alcohol~quality) # looks positive linear
boxplot(volatile.acidity~quality) # possibly negative linear

boxplot(sulphates~quality) # fairly horizontal
boxplot(density~quality) # fairly horizontal
boxplot(residual.sugar~quality) # fairly horizontal

boxplot(pH~quality) # seems almost constant
boxplot(total.sulfur.dioxide~quality) # seems almost constant
boxplot(free.sulfur.dioxide~quality) # seems almost constant
boxplot(chlorides~quality) # seems almost constant
boxplot(citric.acid~quality)  # seems almost constant
boxplot(fixed.acidity~quality) # seems almost constant

ggplot(aes(x=alcohol, y = quality), data = red) + 
  geom_point(aes(colour = quality), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs Alcohol")+
  scale_colour_gradient(low = "blue", high = "green")

ggplot(aes(x=volatile.acidity, y = quality), data = red) + 
  geom_point(aes(colour = quality), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs Volatile Acidity")+
  scale_colour_gradient(low = "yellow", high = "red")

ggplot(aes(x=sulphates, y = quality), data = red) + 
  geom_point(aes(colour = quality), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs Sulphates")+
  scale_colour_gradient(low = "purple", high = "cyan")

ggplot(aes(x=density, y = quality), data = red) + 
  geom_point(aes(colour = quality), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs Density")+
  scale_colour_gradient(low = "green", high = "red")

ggplot(aes(x=residual.sugar, y = quality), data = red) + 
  geom_point(aes(colour = quality), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs Residual Sugar")+
  scale_colour_gradient(low = "blue", high = "orange")

ggplot(aes(x=pH, y = quality), data = red) + 
  geom_point(aes(colour = quality), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs pH")+
  scale_colour_gradient(low = "blue", high = "orange")

# Simple Linear Regression of Quality by Alcohol
SLR_alcohol <- lm(quality~alcohol)
SLR_alcohol
summary(SLR_alcohol)
plot(quality~alcohol, main='Quality by Alcohol', xlab='Alcohol', ylab='Quality')
abline(SLR_alcohol, col='cyan')
# residual plot
plot(SLR_alcohol$fitted.values,SLR_alcohol$residuals, main="Residuals: Quality by Alcohol", xlab='Fitted Values', ylab='Residuals')
abline(h=0,col="red")

# Simple Linear Regression of Quality by Volatile Acidity
SLR_volatile.acidity <- lm(quality~volatile.acidity)
SLR_volatile.acidity
summary(SLR_volatile.acidity)
plot(quality~volatile.acidity, main='Quality by Volatile Acidity', xlab='Volatile Acidity', ylab='Quality')
abline(SLR_volatile.acidity, col='cyan')
# residual plot
plot(SLR_volatile.acidity$fitted.values,SLR_volatile.acidity$residuals, main="Residuals: Quality by Volatile Acidity", xlab='Fitted Values', ylab='Residuals')
abline(h=0,col="red")

SLR_residual.sugar <- lm(quality~residual.sugar)
SLR_residual.sugar
summary(SLR_residual.sugar)
plot(quality~residual.sugar, main='Quality by residual.sugar', xlab='residual.sugar', ylab='Quality')
abline(SLR_residual.sugar, col='cyan')
# residual plot
plot(SLR_residual.sugar$fitted.values,SLR_residual.sugar$residuals, main="Residuals: Quality by pH", xlab='Fitted Values', ylab='Residuals')
abline(h=0,col="red")

set.seed(800)

##choose the observations to be in the training. I am splitting the dataset into halves
sample<-sample.int(nrow(red), floor(.50*nrow(red)), replace = F)
train<-red[sample, ]
test<-red[-sample, ]
