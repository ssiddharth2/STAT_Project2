white_wine_data<-read.csv("wineQualityWhites.csv", header = TRUE, sep=',')

summary(white_wine_data$fixed.acidity)
summary(white_wine_data$volatile.acidity)
summary(white_wine_data$citric.acid)
summary(white_wine_data$residual.sugar)
summary(white_wine_data$chlorides)
summary(white_wine_data$alcohol)
summary(white_wine_data$quality)

#Work In Progress