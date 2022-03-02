#BORRA LOS DATOS DE ENVORONMENT
rm(list=ls(all=TRUE)) 


#una forma elengate de instalar paquetes y usar las librerias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(survival, KMsurv, survMisc, survminer, flexsurv, dplyr, gridExtra )

#Nuestrso datos para el ejemplo
X<-c(0.7, 1.1, 11, 1.6, 4, 7.1, 14.4, 40)
Bueno<-c(0.2, 0.4, 2.5, 0.4, 0.8, 1.6, 2.9, 8)
Malo<-c(0.2, 0.4, 2.5, 0.4, 0.8, 1.6, 2.9, 2)

mis_datos<-data.frame(x, Bueno, Malo)   # Haciendo el dataframe

attach(mis_datos)                        # Para elegir libremente nuestras variables
print(mis_datos)                         # ver mis datos

#RESUMEN DE LOS DATOS
summary(mis_datos)                       # resumen de mis datos

par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto

#HACIENDO LA FIGURA DE DISPERSION DEL BUENO
plot(X, Bueno, xlab="Precipitación total (cm)", col.lab=3, ylab="Intercepción (cm)", pch=1, col="green", lwd = 5, col.axis=3)
abline(lm(Bueno~X), col="green", col.axis=1)

#HACIENDO LA FIGURA DE DISPERSION DEL MALO
plot(X, Malo, xlab="Precipitación total (cm)", col.lab=2, ylab="Intercepción (cm)", pch=1, col="red", lwd = 5, col.axis=2)
abline(lm(Malo~X), col="red")

par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto
#boxplot del bueno la variable y
boxplot(Bueno, main="El Bueno", col="green")
abline(h=median(Bueno)); abline(h=mean(Bueno), col="green") #Plot de la media y mediana

#boxplot del malo
boxplot(Malo, main="El Malo", col="red")
abline(h=median(Malo)); abline(h=mean(Malo), col="red")     # #Plot de la media y mediana

#Prueba de normaliad de los datos   USANDO GGPLOT
p1<-ggqqplot(Bueno, ylab = "Bueno", conf.int = TRUE, conf.int.level = 0.95, color = "green")
p2<-ggqqplot(Malo, ylab = "Malo", conf.int = TRUE, conf.int.level = 0.95, color = "red")
grid.arrange(p1, p2, nrow = 1)   # para colocar dos plost en una fila

# Prueba de hipótesis de correlacion H0:rho=0 (Prueba de hipotesis) Bueno
cor.test(X, Bueno, alternative="two.sided", method="pearson")

# Prueba de hipótesis de correlacionH0:rho=0 (Prueba de hipotesis) Malo
cor.test(X, Malo, alternative="two.sided", method="pearson")


par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto
#LA REGRESION del bueno
  n<-length(X)
  # Grafica
  plot(X, Bueno, ylab="Intercepción (mm", xlab="Precipitación total (mm)", main="Diagrama de dispersión", col.lab=3)

# Estimación de parámetros y prueba de hipótesis
fit_Bueno<-lm(Bueno~X); summary(fit_Bueno)
abline(fit_Bueno,col="green")

# Podemos obtener el valor predicho de Y para cada valor de X
fitted_bueno<-fit_Bueno$ fitted.value; fitted_bueno
# Con un bucle for y la función lines() podemos representar los resíduos
for(i in 1:n)
{
  lines( c(X[i],X[i]), c(Bueno[i], fitted_bueno[i]), col="green")
}


#$$$$$$$$$$$$$$$$$$$$$$$$$$
#LA REGRESION del malo
n<-length(X)
# Grafica
plot(X, Malo, ylab="Intercepción (mm", xlab="Precipitación total (mm)", main="Diagrama de dispersión", col.lab=2)

# Estimación de parámetros y prueba de hipótesis
fit_Malo<-lm(Malo~X); summary(fit_Malo)
abline(fit_Malo,col="red")


# Podemos obtener el valor predicho de Y para cada valor de X
fitted_malo<-fit_Malo$ fitted.value; fitted_malo

# Con un bucle for y la función lines() podemos representar los resíduos
for(i in 1:n)
{
  lines( c(X[i],X[i]), c(Malo[i], fitted_malo[i]), col="red")
}
############################################################33


# Intervalos de confianza del bueno
confint(fit_Bueno)

# Intervalos de confianza del Malo
confint(fit_Malo)


#%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%

par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto
# Residuos ordinarios del Bueno
fit_Bueno$residuals        # de aqui sacamos los residuales
boxplot(fit_Bueno$residuals, col="green", xlab="Bueno", ylab="Residuales ordianrios",
        ylim=c(-2, 2))

# Residuos ordinarios del Malo
fit_Malo$residuals
boxplot(fit_Malo$residuals, col="red",  xlab="malo", ylab="Residuales ordianrios",
        ylim=c(-2, 2))

#####################################
# Prueba de normalidad de residuales de ambos modelos
shapiro.test(fit_Bueno$residuals) #Bueno
shapiro.test(fit_Malo$residuals) #Malo


# Cuantiles del Bueno
par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto
qqnorm(fit_Bueno$residuals)
qqline(fit_Bueno$residuals, col="green")

# Cuantiles del Malo
qqnorm(fit_Malo$residuals)
qqline(fit_Malo$residuals, col="red")

#%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%
# Prueba de Durbin -Watson del Bueno
library(lmtest, pos=4)
dwtest(Bueno ~ X, alternative="two.sided")

# Prueba de Durbin -Watson del Malo
library(lmtest, pos=4)
dwtest(Malo ~ X, alternative="two.sided")

############## Correlograma de residuales
par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto
#Correlograma de resduales del bueno
acf(fit_Bueno$ residuals, col="green")

#Correlograma de resduales del malo
acf(fit_Malo$ residuals, col="red")


#%%%%%%%%%%%%%%%%%%%%%%%%55
# Residuos frente a puntuaciones ajustadas Bueno
oldpar <- par(oma=c(0,0,3,0), mfrow=c(1,2))
plot(fit_Bueno)
par(oldpar)


# Residuos frente a puntuaciones ajustadas Malo
oldpar <- par(oma=c(0,0,3,0), mfrow=c(1,2))
plot(fit_Malo)
par(oldpar)

######### PRUEBA DE HOMOGENEIDAD DE VARIANZA
# Prueba de Breusch-Pagan para la heteroscedasticidad del Bueno
a<-bptest(Bueno ~ X, varformula = ~ fitted.values(fit_Bueno), studentize=FALSE); a

# Prueba de Breusch-Pagan para la heteroscedasticidad del Bueno
a<-bptest(Malo ~ X, varformula = ~ fitted.values(fit_Malo), studentize=FALSE); a

# Observaciones influyentes y atípicos
############## RESIDUOS ESTANDARIZADOS
# Residuos estandarizados Bueno
par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto
ei=fit_Bueno$residuals/summary(fit_Bueno)$sigma
boxplot(ei, col="green", xlab="Bueno", ylim=c(-2, 2))

# Residuos estandarizados Malo
ei=fit_Malo$residuals/summary(fit_Malo)$sigma
boxplot(ei, col="red", xlab="malo", ylim=c(-2, 2))


############## RESIDUOS ESTUDENTIZADOS
par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto
# Residuos ESTUDENTIZADOS del Bueno
ri=rstudent(fit_Bueno)
boxplot(ri, col="green", ylim=c(-20, 20))
data.frame(ei,ri)
par(new=T)
abline(h=3, col="red");abline(h=-3, col="red")


# Residuos estudentizados del Malo
ri=rstudent(fit_Malo)
boxplot(ri, col="red", ylim=c(-20, 20))
data.frame(ei,ri)
par(new=T)
abline(h=3, col="red");abline(h=-3, col="red",labels="T")



# Estadísticos y graficas de diagnóstico del Bueno
par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto
inflm.SR=influence.measures(fit_Bueno); summary(inflm.SR)
library(car)
outlierTest(fit_Bueno)
influencePlot(fit_Bueno, col="green")

# Estadísticos y graficas de diagnóstico del malo
inflm.SR=influence.measures(fit_Malo); summary(inflm.SR)
library(car)
outlierTest(fit_Malo)
influencePlot(fit_Malo, col="red")


##################### BANDAS DE CONFIANZA DE LAS PREDICCIONES

# Banda de confianza para E(Y/X0) del Bueno
new <- data.frame(X=seq(from=min(X), to=max(X), length=X)); new
CI95 <- predict( fit_Bueno, newdata=new, se.fit_Bueno=TRUE,interval="confidence", level=0.95)
CI95

par(mfrow=c(1,2))      # dos solo gráfico por ventana: la opción por defecto
# Gráfica Bueno
X0<-seq(min(X),max(X),length=length(Bueno))
pred.m<-predict(fit_Bueno,data.frame(X=X0),interval="confidence",se.fit=T)
par(mfrow=c(1,1))
matplot(X0,cbind(pred.m$fit),lty=c(1,2,2,3,3),
        col=c("black","red","red","blue","blue"),type="l",xlab="Precipitación total (cm)",
        ylab="Intercepcion (cm)")
points(X,Bueno)

#$$$$$$$$$$$$$$$$$$$$$$$$
# Banda de confianza para E(Y/X0) Malo
new <- data.frame(X=seq(from=min(X), to=max(X), length=X)); new
CI95 <- predict( fit_Malo, newdata=new, se.fit_Malo=TRUE,interval="confidence", level=0.95)
CI95

# Gráfica Malo
X0<-seq(min(X),max(X),length=length(Malo))
pred.m<-predict(fit_Malo,data.frame(X=X0),interval="confidence",se.fit=T)
par(mfrow=c(1,1))
matplot(X0,cbind(pred.m$fit),lty=c(1,2,2,3,3),
        col=c("black","red","red","blue","blue"),type="l",xlab="Precipitación total (cm)",
        ylab="Intercepcion (cm)")
points(X,Malo)


