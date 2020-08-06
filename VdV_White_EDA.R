### Vino da 'Ville
### Michael Kolonay (mhk9c)
### Jordan Machita (jm8ux)
### Stephen Morris (sam3ce)
### Siddharth Surapeneni (sss2e)

### White Wine Exploratory Data Analysis

library(ggplot2)
library(ROCR)

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



# box plots of binary quality rating for EDA
dataW

#Need this line to get the boxplots right for ggplot.
#make sure it works ok for regressions.
dataW$qualR <- as.factor(dataW$qualR)

levels(dataW$qualR) <- c("low", "high") 
contrasts(dataW$qualR)


ggplot(data=dataW, aes(qualR,fill=qualR)) + 
  geom_bar()  +
  ggtitle("High Quality and Low Quality White Wines") +
  geom_text(stat='count', nudge_y=-100,  aes(label=..count..)) +
  scale_fill_brewer(palette="Dark2")



#####################
#alcohol
####################
boxplot(dataW$alcohol~dataW$qualR) # looks useful

ggplot(dataW, aes(x=qualR, y=alcohol, fill=qualR)) + 
  ggtitle("High and Low Quality \n White Wines by Alcohol") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

#####################
#fixed.acidity
####################
boxplot(dataW$fixed.acidity~dataW$qualR) # definitely not

ggplot(dataW, aes(x=qualR, y=fixed.acidity, fill=qualR)) + 
  ggtitle("High and Low Quality \n White Wines by Fixed Acidity") +
    geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

#####################
#volatile.acidity
####################
boxplot(dataW$volatile.acidity~dataW$qualR) # probs not

ggplot(dataW, aes(x=qualR, y=volatile.acidity, fill=qualR)) + 
  ggtitle("High and Low Quality \n White Wines by Volatile Acidity") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

#####################
#citric.acid
####################
boxplot(dataW$citric.acid~dataW$qualR) # maybe?
ggplot(dataW, aes(x=qualR, y=citric.acid, fill=qualR)) +
  ggtitle("High and Low Quality \n White Wines by Citric Acid") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

#####################
#residual.sugar
####################
boxplot(dataW$residual.sugar~dataW$qualR)# possibly
ggplot(dataW, aes(x=qualR, y=residual.sugar, fill=qualR)) + 
  ggtitle("High and Low Quality \n White Wines by Residual Sugar") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

#####################
#chlorides
####################
boxplot(dataW$chlorides~dataW$qualR) # probably
ggplot(dataW, aes(x=qualR, y=chlorides, fill=qualR)) + 
  ggtitle("High and Low Quality \n White Wines by Chlorides") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

#####################
#free.sulfur.dioxide
####################
boxplot(dataW$free.sulfur.dioxide~dataW$qualR) # nope
ggplot(dataW, aes(x=qualR, y=free.sulfur.dioxide, fill=qualR)) + 
  ggtitle("High and Low Quality \n White Wines by Free Sulfur Dioxide") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

#####################
#total.sulfur.dioxide
####################
boxplot(dataW$total.sulfur.dioxide~dataW$qualR) # maybe
ggplot(dataW, aes(x=qualR, y=total.sulfur.dioxide, fill=qualR)) + 
  ggtitle("High and Low Quality \n White Wines by Total Sulfur Dioxide") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

#####################
#density
####################
boxplot(dataW$density~dataW$qualR) # maybe
ggplot(dataW, aes(x=qualR, y=density, fill=qualR)) + 
  ggtitle("High and Low Quality \n White Wines by Total Density") +
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  scale_fill_brewer(palette="Dark2")

#####################
#pH
####################
boxplot(dataW$pH~dataW$qualR) # nope
ggplot(dataW, aes(x=qualR, y=pH, fill=qualR)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  ggtitle("High and Low Quality \n White Wines by pH") +
  scale_fill_brewer(palette="Dark2")

#####################
#sulphates
####################
boxplot(dataW$sulphates~dataW$qualR) # nope
ggplot(dataW, aes(x=qualR, y=sulphates, fill=qualR)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8) +
  ggtitle("High and Low Quality \n White Wines by Sulphates") +
  scale_fill_brewer(palette="Dark2")

