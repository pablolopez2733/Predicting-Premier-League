#validacion de supuestos y modelos de dos variables
library(fmsb)
library(dplyr)
data<-read.csv(file.choose(),header=T, sep=",")
value<-data[,2]
Fifa<-data[,3]
Rating<-data[,4]
Avg90<-data[,5]
Goal<-data[,6]
Points<-data[,7]
Wins<-data[,8]
PPG<-data[,11]
pairs(data) #grafica de variable vs variable (hacer zoom)
#points ~ value + Fifa
correlacionVF<- cor(value,Fifa) #correlación "mala", arriba de .8
modelo1<-lm(Points~value+Fifa) #Residuals vs Fitted varianza no constante, muchos outliers en grafica normal Q-Q
summary(modelo1) 
plot(modelo1)
VIF(modelo1)
confint(lm(formula = Points ~ value+Fifa)) #intervalos de confianza
E1<-residuals(modelo1) #mucha variabilidad, modelo muy piñata
var(E1)
mean(E1) #cercana a 0 
fitted(modelo1) #son malos 
summary(influence.measures(modelo1)) #observaciones mas influyentes

#points ~ value + rating
correlacionVR<- cor(value,Rating) #correlación "buena", abajo de .8
modelo2<-lm(Points~value+Rating) #Residuals vs Fitted varianza sigue siend no constante, muchos outliers en grafica normal Q-Q
summary(modelo2) 
plot(modelo2)
VIF(modelo2)
confint(lm(formula = Points ~ value+Rating)) #intervalos de confianza
E2<-residuals(modelo2) #mucha variabilidad, modelo muy piñata
var(E2)
mean(E2) #cercana a 0 
fitted(modelo2) #son malos, sin embargo son mejores que modelo1 
summary(influence.measures(modelo2)) #observaciones mas influyentes
#points ~ value + Avg90
correlacionVA<- cor(value,Avg90) #correlación "buena", abajo de .8
modelo3<-lm(Points~value+Avg90) #Residuals vs Fitted la varianza mejoro, parece mas cosntante a diferencia de los otros modelos, muchos outliers en grafica normal Q-Q
summary(modelo3) 
plot(modelo3)
VIF(modelo3)
confint(lm(formula = Points ~ value+Avg90)) #intervalos de confianza, podriamos decir que value es insignificante porque 0 esta en el IC, hay que comparar contra el modelo donde solo se toma Avg90
E3<-residuals(modelo3) #mucha variabilidad, modelo muy piñata
var(E3) #la varianza mejoro
mean(E3) 
fitted(modelo3) #son decentes, hasta ahora el mejor modelo
summary(influence.measures(modelo3)) #observaciones mas influyentes
#points ~ value + Goal
correlacionVG<- cor(value,Goal) #correlación "buena", abajo de .8
modelo4<-lm(Points~value+Goal) #Residuals vs Fitted la varianza nos es constante, muchos outliers en grafica normal Q-Q, no parece normal
summary(modelo4) 
plot(modelo4)
VIF(modelo4)
confint(lm(formula = Points ~ value+Goal)) #intervalos de confianza, podriamos decir que value es insignificante porque 0 esta en el IC, pero aun así este modelo es malo
E4<-residuals(modelo4) #mucha variabilidad, modelo muy piñata
var(E4) #la varianza es alta
mean(E4) 
fitted(modelo4) #hay mucha variabilidad, hay algunos buenos y otros muy malos
summary(influence.measures(modelo4)) #observaciones mas influyentes
#points ~ value + Wins
correlacionVW<- cor(value,Wins) #correlación "buena", abajo de .8
modelo5<-lm(Points~value+Wins) #Residuals vs Fitted la varianza nos es constante, pero la grafica normal Q-Q muestra que los residuos son normales
summary(modelo5) 
plot(modelo5)
VIF(modelo5)
confint(lm(formula = Points ~ value+Wins)) #intervalos de confianza, podriamos decir que value es insignificante porque 0 esta en el IC, lo cual tiene sentido ya que una victoria influye mas a los puntos
E5<-residuals(modelo5) 
var(E5) #la varianza es baja
mean(E5) 
fitted(modelo5) #los fitted values son buenos, sin embargo la varianza no es constante :(, tal vez agregando mas variables se logre hacer la varianza constante
summary(influence.measures(modelo5)) #observaciones mas influyentes
#points ~ Fifa+ Wins
correlacionFW<- cor(Fifa,Wins) #correlación "buena", abajo de .8
modelo6<-lm(Points~Fifa+Wins) #Residuals vs Fitted la varianza no se ve muy constante, pero la grafica normal Q-Q muestra que los residuos son normales
summary(modelo6) 
plot(modelo6)
VIF(modelo6)
confint(lm(formula = Points ~ Fifa+Wins)) #intervalos de confianza, podriamos decir que Fifa  no es significa porque 0 esta en el IC
E6<-residuals(modelo6) #residuos pequeños
var(E6) #la varianza es baja
mean(E6) 
fitted(modelo6) #los fitted values son buenos, sin embargo la varianza no es constante :(, tal vez agregando mas variables se logre hacer la varianza constante
summary(influence.measures(modelo6)) #observaciones mas influyentes
#points ~ Fifa+ Goal
correlacionFG<- cor(Fifa,Goal) #correlación "buena", abajo de .8
modelo7<-lm(Points~Fifa+Goal) #Residuals vs Fitted la varianza no se constante, la grafica normal Q-Q muestra que los residuos no son normales
summary(modelo7) 
plot(modelo7)
VIF(modelo7)
confint(lm(formula = Points ~ Fifa+Goal)) #intervalos de confianza
E7<-residuals(modelo7) #residuos pequeños
var(E7) #la varianza es alta
mean(E7) 
fitted(modelo7) #los fitted values son muy variables, algunos buenos algunos malos
summary(influence.measures(modelo7))
#points ~ Fifa+ Avg90
correlacionFA<- cor(Fifa,Avg90) #correlación "buena", abajo de .8
modelo8<-lm(Points~Fifa+Avg90) #Residuals vs Fitted la varianza es constante, la grafica normal Q-Q muestra que los residuos son normales, pero con sus outliers
summary(modelo8) 
plot(modelo8)
VIF(modelo8)
confint(lm(formula = Points ~ Fifa+Avg90)) #intervalos de confianza, fifa se puede descartar
E8<-residuals(modelo8) #residuos pequeños
var(E8) #la varianza es "baja"
mean(E8) 
fitted(modelo8) #los fitted values son decentes
summary(influence.measures(modelo8))
#points ~ Fifa+ Rating
correlacionFR<- cor(Fifa,Rating) #correlación "buena", abajo de .8
modelo9<-lm(Points~Fifa+Rating) #Residuals vs Fitted la varianza no es constante, la grafica normal Q-Q muestra que los residuos son normales, pero con sus outliers
summary(modelo9) 
plot(modelo9)
VIF(modelo9)
confint(lm(formula = Points ~ Fifa+Rating)) #intervalos de confianza
E9<-residuals(modelo9) 
var(E9) #la varianza es alta
mean(E9) 
fitted(modelo9) #los fitted values son malos, muy variables
summary(influence.measures(modelo9))

