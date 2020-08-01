library("tidyverse")
white_wine_data<-read.csv("wineQualityWhites.csv", header = TRUE, sep=',')

summary(white_wine_data$fixed.acidity)#Potential Categorical
summary(white_wine_data$volatile.acidity)
summary(white_wine_data$citric.acid)
summary(white_wine_data$residual.sugar)
summary(white_wine_data$chlorides)


summary(white_wine_data$alcohol)
summary(white_wine_data$quality)
summary(white_wine_data$pH)
summary(white_wine_data$sulphates)


#Work In Progress

plot(quality~fixed.acidity, data=white_wine_data)

#While the medians for each of the box plots are similar,
#the ranges for acidity of a white wine in a boxplot get narrower as the quality 
#of the wine increases
boxplot(fixed.acidity~quality,data=white_wine_data)

#While the medians for each of the box plots are similar,
#the ranges for acidity of a white wine in a boxplot get narrower as the quality 
#of the wine increases
cut1 <- subset(white_wine_data,quality==9 | quality == 8)


summary(cut1$fixed.acidity) #Fixed Acidity for 50% of High Quality Wines are between 6.2 and 7.3

plot(quality~volatile.acidity, data=white_wine_data) #Straight Line Patterns suggest Logistic Regression

#While the medians for each of the box plots are similar,the ranges for each of the boxplots appear to be similar
boxplot(volatile.acidity~quality,data=white_wine_data)

plot(quality~citric.acid, data=white_wine_data) #Straight Line Patterns suggest Logistic Regression

#Wider ranges for wines with quality ratings of 5 and 6, low quality wine and high quality wine appear to be 
#similar to each other
boxplot(citric.acid~quality,data=white_wine_data)

plot(quality~residual.sugar, data=white_wine_data) #Straight Line Patterns suggest Logistic Regression

#Residuals Sugar seem to be higher in medium quality(5-6 rating) wines, they have wider ranges as well
boxplot(residual.sugar~quality,data=white_wine_data)

plot(quality~chlorides, data=white_wine_data) #Straight Line Patterns suggest Logistic Regression

#Chloride levels seem to get lower as the quality rating of the wine increases
boxplot(chlorides~quality,data=white_wine_data)

plot(quality~chlorides, data=white_wine_data) #Straight Line Patterns suggest Logistic Regression

#Chloride levels seem to get lower as the quality rating of the wine increases
boxplot(chlorides~quality,data=white_wine_data)

plot(quality~alcohol, data=white_wine_data)#Straight Line Patterns suggest Logistic Regression

#Alcohol levels rise as the quality rating of the wine increases
boxplot(alcohol~quality,data=white_wine_data)

plot(quality~pH, data=white_wine_data)#Straight Line Patterns suggest Logistic Regression

#As the quality rating of the wine increases, the median pH seems to increases
boxplot(pH~quality,data=white_wine_data)

plot(quality~sulphates, data=white_wine_data)#Straight Line Patterns suggest Logistic Regression

#As the quality rating of the wine increases, the median pH seems to increases
boxplot(sulphates~quality,data=white_wine_data)

boxplot(free.sulfur.dioxide~quality,data=white_wine_data)

plot(quality~free.sulfur.dioxide, data=white_wine_data)

boxplot(total.sulfur.dioxide~quality,data=white_wine_data)

plot(quality~total.sulfur.dioxide, data=white_wine_data)


white_wine_fixed_acidity<-white_wine_data %>%
  group_by(quality)%>%
  summarise(mean=mean(fixed.acidity), median=median(fixed.acidity))
white_wine_fixed_acidity

white_wine_volatile_acidity<-white_wine_data %>%
  group_by(quality)%>%
  summarise(mean=mean(volatile.acidity), median=median(volatile.acidity))
white_wine_fixed_acidity

white_wine_citric_acid<-white_wine_data %>%
  group_by(quality)%>%
  summarise(mean=mean(citric.acid), median=median(citric.acid))
white_wine_citric_acid #Wine means appear to be similar except at the ends(quality rating 3 and quality rating 9)

white_wine_residual_sugar<-white_wine_data %>%
  group_by(quality)%>%
  summarise(mean=mean(residual.sugar), median=median(residual.sugar))
white_wine_residual_sugar #Wines with quality ratings of 5 and 6 seem to have higher residual sugars

white_wine_chlorides<-white_wine_data %>%
  group_by(quality)%>%
  summarise(mean=mean(chlorides), median=median(chlorides))
white_wine_chlorides #Decrease as the quality rating increases

white_wine_pH<-white_wine_data %>%
  group_by(quality)%>%
  summarise(mean=mean(pH), median=median(pH))
white_wine_pH #Average mmean and median increase as the quality rating increases

white_wine_alcohol<-white_wine_data %>%
  group_by(quality)%>%
  summarise(mean=mean(alcohol), median=median(alcohol))
white_wine_alcohol #Average mean and median increase as the quality rating increase

white_wine_sulphates<-white_wine_data %>%
  group_by(quality)%>%
  summarise(mean=mean(sulphates), median=median(sulphates))
white_wine_sulphates #Seem to be similar, not very clear what relationship is

white_wine_density<-white_wine_data %>%
  group_by(quality)%>%
  summarise(mean=mean(density), median=median(density))
white_wine_density

#Potential predictors: pH, chlorides, and alcohol
white_wine_alcohol<-white_wine_data %>%
  group_by(quality)%>%
  summarise(count=count(quality))

