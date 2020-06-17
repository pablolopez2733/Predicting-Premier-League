#-----------------------------------
#Predicciones para la Premier League 
#-----------------------------------

#Libraries


#Downloading Data
url<- 'https://github.com/pablolopez2733/Predicting-Premier-League/raw/master/CleanData/TeamMetricsFinal.csv'
df<-read.csv(url)

#Setting up variables for LR
seasons <- data.frame("teams" = df$team,"xG" = df$Avg90,"wsRating" = df$AvgRating,"Points"=df$Points)
seasons$logPoints=log(seasons$Points)

#Linear Regression------------------
mod <- lm(logPoints ~ xG,data = seasons)
summary(mod)

wsmod <- lm(logPoints ~ wsRating,data = seasons)
summary(wsmod)


#Predictions------------------------
url2<- 'https://github.com/pablolopez2733/Predicting-Premier-League/raw/master/CleanData/1920metrics.csv'
premier19<-read.csv(url2)

xGvalues <- data.frame(xG = premier19$xG)
predictionsxG <- predict(mod, newdata=xGvalues)

predicciones <- data.frame(as.list(predictionsxG))
predicciones <- as.data.frame(t(predicciones))
predicciones$Puntos = exp(predicciones$V1)

