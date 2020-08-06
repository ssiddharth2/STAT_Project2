### Vino da 'Ville
### Michael Kolonay (mhk9c)
### Jordan Machita (jm8ux)
### Stephen Morris (sam3ce)
### Siddharth Surapeneni (sss2e)

### Red Wine Exploratory Data Analysis

library(MASS)
library(RColorBrewer)
library(ggplot2)
library(nnet)
library(tidyverse)

red<-read.csv("wineQualityReds.csv", header=TRUE)
white<-read.csv("wineQualityWhites.csv", header=TRUE)
attach(red)

for (row in 1:nrow(red)) {
  if (red[row, "quality"] > 6) {
    red[row, "qualR"] <- 1
  } else {
    red[row, "qualR"] <- 0
  }
}

red$qualR <- as.factor(red$qualR)
levels(red$qualR) <- c("low", "high") 
contrasts(red$qualR)
### Exploratory Data Analysis

### Alcohol

boxplot(alcohol~quality) # looks positive linear

ggplot(red, aes(x=quality, y=alcohol, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Alcohol") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=alcohol, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Alcohol") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")


### Volatile Acidity

boxplot(volatile.acidity~quality) # possibly negative linear

ggplot(red, aes(x=quality, y=volatile.acidity, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Volatile Acidity") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=volatile.acidity, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Volatile Acidity") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

### Sulphates
boxplot(red$sulphates~red$quality) # fairly horizontal

ggplot(red, aes(x=quality, y=sulphates, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Sulphates") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=sulphates, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Sulphates") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

### Density
boxplot(density~quality) # fairly horizontal

ggplot(red, aes(x=quality, y=density, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Density") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=density, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Density") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

### Citric Acid
boxplot(red$citric.acid~red$quality)  # fairly horizontal

ggplot(red, aes(x=quality, y=citric.acid, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Citric Acid") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=citric.acid, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Citric Acid") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

### Fixed Acidity
boxplot(fixed.acidity~quality) # seems almost constant

ggplot(red, aes(x=quality, y=fixed.acidity, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Fixed Acidity") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=fixed.acidity, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Fixed Acidity") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

 ### Residual Sugar
boxplot(red$residual.sugar~red$quality) # fairly horizontal

ggplot(red, aes(x=quality, y=residual.sugar, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Residual Sugar") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=residual.sugar, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Residual Sugar") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")
### pH
boxplot(pH~quality) # seems almost constant

ggplot(red, aes(x=quality, y=pH, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by pH") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=pH, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by pH") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")
### Chlorides
boxplot(chlorides~quality)

ggplot(red, aes(x=quality, y=chlorides, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Chlorides") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=chlorides, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Chlorides") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

### Total Sulfur Dioxide
boxplot(total.sulfur.dioxide~quality) # seems almost constant

ggplot(red, aes(x=quality, y=total.sulfur.dioxide, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Total Sulfur Dioxide") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

ggplot(red, aes(x=qualR, y=total.sulfur.dioxide, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Total Sulfur Dioxide") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

### Free Sulfur Dioxide
boxplot(free.sulfur.dioxide~quality) # seems almost constant

ggplot(red, aes(x=quality, y=free.sulfur.dioxide, group=quality)) +  
  ggtitle("High and Low Quality \n Red Wines by Free Sulfur Dioxide") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

### Line Plots
ggplot(red, aes(x=qualR, y=free.sulfur.dioxide, fill=qualR)) + 
  ggtitle("High and Low Quality \n Red Wines by Free Sulfur Dioxide") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")


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

ggplot(aes(x=citric.acid, y = quality), data = red) + 
  geom_point(aes(colour = quality), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs citric acid")+
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
