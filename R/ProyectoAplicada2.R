#Install Packages
install.packages("ggstatsplot")
install.packages("e1071")

#Loading Packages to Be Used
library(fmsb)
library(ggstatsplot)
library(e1071) 

#Read Data
data<-read.csv(file.choose(),header=T, sep=",")
value<-data[,2]
Fifa<-data[,3]
Rating<-data[,4]
Avg90<-data[,5]
Goal<-data[,6]
Points<-data[,7]
Wins<-data[,8]
PPG<-data[,11]

#First Model to analyze:
#1. Points ~ Value

# I) Run a basic scatterplot
scatter.smooth(x=value, y=Points, main="Points ~ Value") 
#Analisis doesnt seem linear 
# II) Run a second attempt with ln
scatter.smooth(x=value, y=log(Points), main="Points ~ Value") 

#Check For outliers:
boxplot(value)$out
boxplot(Points)

#Eliminate Outliers
Q <- quantile(value, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(value)
eliminated<- subset(data, value > (Q[1] - 1.5*iqr) & value < (Q[2]+1.5*iqr))
eliminated_value <- eliminated[,2]
eliminated_Fifa<- eliminated[,3]
eliminated_Rating<- eliminated[,4]
eliminated_90<-eliminated[,5]
eliminated_Points<- eliminated[,7]


#Check Scatterplot Again
scatter.smooth(x=eliminated_value, y=eliminated_Points, main="Points ~ Value")


#Produce LM
Model_PV <-lm(eliminated_Points~eliminated_value)
#Observe Results
summary(Model_PV)
#Plot
plot(Model_PV)


#Check for Residuals
E_PV<-residuals(Model_PV)
var(E_PV)
mean(E_PV)

#Much more linear, now we check normality through Density Plot
plot(density(E_PV), main="Density Plot: Residuals", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(E_PV), 2)))  # density plot for Points
polygon(density(E_PV), col="red")


#Second Model to analyze:
#2. Points ~ Fifa
scatter.smooth(x=Fifa, y=log(Points), main="Points ~ Fifa") 
#Seems linear, lets check for outliers
boxplot(Fifa)$out #No Outliers

#Produce LM
Model_PF <-lm(log(Points)~Fifa)
#Observe Results
summary(Model_PF)
#Plot
plot(Model_PF)

#Check for Residuals
E_PF<-residuals(Model_PF)
var(E_PF)
mean(E_PF)

#Much more linear, now we check normality through Density Plot
plot(density(E_PF), main="Density Plot: Residuals", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(E_PF), 2)))  # density plot for Points
polygon(density(E_PF), col="green")

#Third Model to Analyze
scatter.smooth(x=Avg90, y=log(Points), main="Points ~ Avg90") 
#Seems linear, lets check for outliers
boxplot(Avg90)$out #No Outliers

#Produce LM
Model_P90 <-lm(log(Points)~Avg90)
#Observe Results
summary(Model_P90)
#Plot
plot(Model_P90)

#Check for Residuals
E_90<-residuals(Model_P90)
var(E_90)
mean(E_90)

#Much more linear, now we check normality through Density Plot
plot(density(E_90), main="Density Plot: Residuals", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(E_90), 2)))  # density plot for Points
polygon(density(E_90), col="green")

#Fourth Model to Analyze
Rating<-Rating*10
scatter.smooth(x=Rating, y=log(Points), main="Points ~ Rating") 
#Seems linear, lets check for outliers
boxplot(Rating)$out #There are Outliers


#Produce LM
Model_Rating <-lm(log(Points)~Rating)
#Observe Results
summary(Model_Rating)
#Plot
plot(Model_Rating)

#Check for Residuals
E_Rating<-residuals(Model_Rating)
var(E_Rating)
mean(E_Rating)

#Much more linear, now we check normality through Density Plot
plot(density(E_Rating), main="Density Plot: Residuals", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(E_Rating), 2)))  # density plot for Points
polygon(density(E_Rating), col="blue")

#Two by Two
#Points~value+Rating
cor(value,Rating) #<.55

#Produce LM
Model_PVR <-lm(log(eliminated_Points)~eliminated_value+eliminated_Rating)
#Observe Results
summary(Model_PVR)
#Plot
plot(Model_PVR)

#Check for Residuals
E_PVR<-residuals(Model_PVR)
var(E_PVR)
mean(E_PVR)

#Much more linear, now we check normality through Density Plot
plot(density(E_PVR), main="Density Plot: Residuals", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(E_PVR), 2)))  # density plot for Points
polygon(density(E_PVR), col="blue")

#Two by Two
#Points~Fifa+90
cor(eliminated_Fifa,eliminated_Rating) #<.8

#Produce LM
Model_PRF<-lm(log(eliminated_Points)~eliminated_Fifa+eliminated_Rating)
#Observe Results
summary(Model_PRF)
#Plot
plot(Model_PRF)

#Check for Residuals
E_PRF<-residuals(Model_PRF)
var(E_PRF)
mean(E_PRF)

#Much more linear, now we check normality through Density Plot
plot(density(E_PRF), main="Density Plot: Residuals", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(E_PRF), 2)))  # density plot for Points
polygon(density(E_PRF), col="blue")

#Two by Two
#Points~Fifa+90
cor(eliminated_Fifa,eliminated_Rating) #<.8

#Produce LM
Model_PRF<-lm(log(eliminated_Points)~eliminated_Fifa+eliminated_Rating)
#Observe Results
summary(Model_PRF)
#Plot
plot(Model_PRF)

#Check for Residuals
E_PRF<-residuals(Model_PRF)
var(E_PRF)
mean(E_PRF)

#Much more linear, now we check normality through Density Plot
plot(density(E_PRF), main="Density Plot: Residuals", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(E_PRF), 2)))  # density plot for Points
polygon(density(E_PRF), col="blue")





