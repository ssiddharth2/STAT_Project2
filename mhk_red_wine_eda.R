library("tidyverse")
library('psych')
# 
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

data.white<-read.csv("wineQualityWhites.csv", header = TRUE, sep=',')
data.white$type <- 'white'

data.red<-read.csv("TwineQualityWhites.csv", header = TRUE, sep=',')
data.red$type <- 'red'
data.all <- rbind(data.red, data.white)

#set up which set we want to use
data <- data.red


###################COMMENTS ARE FOR RED #############################

#some general data plots
#QQ plots

gather(data, condition, measurement, fixed.acidity:quality, factor_key = TRUE) %>%
  ggplot(aes(sample = measurement)) +
  facet_wrap(~ condition, scales = "free") +
  stat_qq() +
  stat_qq_line()


# Some distributions to note:
# volatile.acidity     
  # Looks pretty heavy tailed skewed right. Might need to transform.
ggplot( data,  aes(x=volatile.acidity)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)
  

# citric.acid  
  # distribution looks pretty bad at higher values.


# residual.sugar
  # distribution looks bad at lower values

# chlorides
  # very heavily skewed right.

# free.sulfur.dioxide
  # some variation at the beginning and end. 

# total.sulfur.dioxide
  #looks o.k. until very high values.

# density
  #pretty good with some outliers - maybe dessert wines?

# pH
  #This looks prett normal

# sulphates
  # Skewed right.

# alcohol
ggplot( data,  aes(x=alcohol)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)
  # kind of all over the place...maybe two different populations here?

# quality
# type
  

#Histograms

multi.hist(data,bcol="red") 

##scatterplot matrix
numeric_data <- data %>% select(fixed.acidity:quality)
pairs(numeric_data, lower.panel = NULL, main="Scatterplot of Quantitative Variables") # huuuugly.

##pairwise correlations
cor_matrix <- round(cor(numeric_data),3)
cor_matrix

# residual.sugar and density
# free.sulfur.dioxide and total.sulfur.dioxide
# density and alcohol



###############################
# fixed.acidity
###############################

##save the plot to a jpg file
# jpeg("joint.jpg")
# dev.off()



ggplot(aes(x=quality, y = fixed.acidity), data = data) + 
  geom_point(aes(colour = fixed.acidity), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs fixed.acidity")

# higher value wines tend to have a tighter band. Seems like they are between 6 and 7.5

fixed.acidity_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(fixed.acidity), median=median(fixed.acidity))
fixed.acidity_mean_median_by_quality

# Trends down until 8, 9 is larger but the sample size is very small.

boxplot(data$fixed.acidity~data$quality,data=data, main="Fixed Acidity by Quality", xlab="Quality", ylab="fixed.acidity")

ggplot( data,  aes(x=fixed.acidity ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)


###############################
# volatile.acidity 
###############################
ggplot(aes(x=quality, y = volatile.acidity), data = data) + 
  geom_point(aes(colour = volatile.acidity), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs volatile.acidity")

# band narrows as quality increases but is not too pronounced between 78

volatile.acidity_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(volatile.acidity), median=median(volatile.acidity))
volatile.acidity_mean_median_by_quality


boxplot(data$volatile.acidity~data$quality,data=data, main="volatile.acidity by Quality", xlab="Quality", ylab="volatile.acidity")

ggplot( data,  aes(x=volatile.acidity ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)

# seems to trend down as quality increases

###############################
# citric.acid
###############################
ggplot(aes(x=quality, y = citric.acid  ), data = data) + 
  geom_point(aes(colour = citric.acid  ), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs citric.acid  ")

# band definitely converges to around .3 as quality increases

citric.acid_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(citric.acid), median=median(citric.acid))
citric.acid_mean_median_by_quality

boxplot(data$citric.acid~data$quality,data=data, main="citric.acid by Quality", xlab="Quality", ylab="citric.acid")

ggplot( data,  aes(x=citric.acid ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)


###############################
# residual.sugar      
###############################
ggplot(aes(x=quality, y = residual.sugar), data = data) +  
  geom_point(aes(colour = residual.sugar), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs residual sugars")

# this one seems all over the place. quality 7,8 seems split, 9 seems low.

residual.sugar_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(residual.sugar), median=median(residual.sugar))
residual.sugar_mean_median_by_quality

boxplot(data$residual.sugar~data$quality,data=data, main="residual.sugar by Quality", xlab="Quality", ylab="residual.sugar")

ggplot( data,  aes(x=residual.sugar ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)


###############################
# chlorides
###############################
ggplot(aes(x=quality, y = chlorides), data = data) + 
  geom_point(aes(colour = chlorides), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs chlorides")

# bands converge around .025 for higher quality wines

chlorides_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(chlorides), median=median(chlorides))
chlorides_mean_median_by_quality

boxplot(data$chlorides~data$quality,data=data, main="chlorides by Quality", xlab="Quality", ylab="chlorides")

ggplot( data,  aes(x=chlorides ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)

###############################
# free.sulfur.dioxide
###############################
ggplot(aes(x=quality, y = free.sulfur.dioxide), data = data) + 
  geom_point(aes(colour = free.sulfur.dioxide), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs free.sulfur.dioxide")

# bands converge to around 33


free.sulfur.dioxide_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(free.sulfur.dioxide), median=median(free.sulfur.dioxide))
free.sulfur.dioxide_mean_median_by_quality


boxplot(data$free.sulfur.dioxide~data$quality,data=data, main="free.sulfur.dioxide by Quality", xlab="Quality", ylab="free.sulfur.dioxide")

ggplot( data,  aes(x=free.sulfur.dioxide ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)


###############################
# total.sulfur.dioxide
###############################
ggplot(aes(x=quality, y = total.sulfur.dioxide), data = data) + 
  geom_point(aes(colour = total.sulfur.dioxide), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs total.sulfur.dioxide")

# band is more spread out but still converges.

total.sulfur.dioxide_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(total.sulfur.dioxide), median=median(total.sulfur.dioxide))
total.sulfur.dioxide_mean_median_by_quality

boxplot(data$total.sulfur.dioxide~data$quality,data=data, main="total.sulfur.dioxide by Quality", xlab="Quality", ylab="total.sulfur.dioxide")

ggplot( data,  aes(x=total.sulfur.dioxide ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)

###############################
# density
###############################
ggplot(aes(x=quality, y = density), data = data) + 
  geom_point(aes(colour = density), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs density")

density_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(density), median=median(density))
density_mean_median_by_quality

boxplot(data$density~data$quality,data=data, main="density by Quality", xlab="Quality", ylab="density")

ggplot( data,  aes(x=density ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)


###############################
# pH
###############################
ggplot(aes(x=quality, y = pH), data = data) + 
  geom_point(aes(colour = pH), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs pH")

pH_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(pH), median=median(pH))
pH_mean_median_by_quality

boxplot(data$pH~data$quality,data=data, main="pH by Quality", xlab="Quality", ylab="pH")

ggplot( data,  aes(x=pH ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)

###############################
# sulphates
###############################
ggplot(aes(x=quality, y = sulphates), data = data) + 
  geom_point(aes(colour = sulphates), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs sulphates")

sulphates_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(sulphates), median=median(sulphates))
sulphates_mean_median_by_quality

boxplot(data$sulphates~data$quality,data=data, main="sulphates by Quality", xlab="Quality", ylab="sulphates")

ggplot( data,  aes(x=sulphates ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)

###############################
# alcohol
###############################
ggplot(aes(x=quality, y = alcohol), data = data) + 
  geom_point(aes(colour = alcohol), alpha = 0.5, size = 2, position = 'jitter')  + 
  ggtitle("Quality vs alcohol")

alcohol_mean_median_by_quality <- data %>%
  group_by(quality)%>%
  summarise(mean=mean(alcohol), median=median(alcohol))
alcohol_mean_median_by_quality

boxplot(data$alcohol~data$quality,data=data, main="alcohol by Quality", xlab="Quality", ylab="alcohol")

ggplot( data,  aes(x=alcohol ,color=quality, fill=quality)) +  
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  facet_wrap(~quality)



# so alcohol, chlorides, citric.acid