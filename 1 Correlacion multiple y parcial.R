data ("airquality")
#https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html

head(airquality)
Datos_0<-as.data.frame(airquality)

library(tidyr)
Datos<-Datos_0 %>% drop_na() #Eliminar celdas con NA

attach(Datos)

str(Datos)                # ver la estructura de los datos

####################################### Analisis exploratorio NUMERICO de los datos
library("pastecs")
round(stat.desc(Datos [1:5]), digits = 2) #Esatadsitica descr

####################################### Analisis exploratorio de los datos
library(DescTools )
PlotMiss(Datos, main="Datos perdidos", clust = FALSE)

####################################### Analisis exploratorio de los datos en plots
library(PerformanceAnalytics)
# Plot correlacion todas las variables  -------------------------------------------
chart.Correlation(Datos[,1:4],   # elegir las variables
                  method="pearson",  # Puede usar otro tipo de correlacion
                  histogram=TRUE,
                  pch=16)

# install.packages("corrplot")
library(corrplot)
head(Datos)
M<-cor(Datos)
head(round(M,4))              # Correlacion de lass variables

corrplot(M, method="circle")  # Plot de correlacion
corrplot(M, method="color")   # Plot de correlacion
corrplot(M, method="number")  # Plot de correlacion

library(qgraph)
Otra_Corr=cor(Datos) 
qgraph(Otra_Corr, shape="circle", posCol="darkgreen", negCol="darkred", 
       layout="spring", vsize=10)


####################################### pruebas de bormalidad

shapiro.test(Datos$Ozone) #  Prueba de normalidad si no es un dataframe
shapiro.test(Solar.R )    #  Prueba de normalidad
shapiro.test(Wind)        #  Prueba de normalidad SI PASA
shapiro.test(Temp)        #  Prueba de normalidad


qqnorm(Ozone, pch=19); qqline(Ozone, col="red")     
qqnorm(Solar.R, pch=19); qqline(Solar.R, col="red")
qqnorm(Wind, pch=19); qqline(Wind, col="red")
qqnorm(Temp, pch=19); qqline(Temp, col="red")

####################################### Visualiando las varoables HISTOGRAMAS

hist(Ozone)               # Histograma de de una variable
hist(Wind)                # Histograma de de una variable

require("lattice")        # historgraa multiple
histogram(~Ozone|as.factor(Month),data=Datos, col="#FF7F50", type = "percent") # temperatura como factor



####################################### Visualiando las varoables HISTOGRAMAS
# Correlacion de dos variables  -------------------------------------------
cor(x=Wind, y=Temp, method = "pearson")
cor(x=Wind, y=Temp, method = "spearman")
cor(x=Wind, y=Temp, method = "kendall")

# Correlacion de dos variables  y su prueba de significancia-------------------------------------------
cor.test(Wind, Temp, method = "spearman")
cor.test(Ozone, Temp, method = "spearman")
head(Datos)
cor.test(Ozone, Solar.R, method = "spearman")    # COn datos ausentes


####################################### Intervalos de confianza de la correlacion
############## Tipos de correlacion
########## https://cran.r-project.org/web/packages/correlation/vignettes/types.html

cor.test(Ozone, Solar.R)    # Veamos una correlacion sin IC
library(correlation)
correlation(Datos)

Pearson <- cor_test(Datos, "Ozone", "Solar.R", method = "pearson"); Pearson
spearman <- cor_test(Datos, "Ozone", "Solar.R", method = "spearman"); spearman
kendall <- cor_test(Datos, "Ozone", "Solar.R", method = "kendall"); kendall
biweight <- cor_test(Datos, "Ozone", "Solar.R", method = "biweight"); biweight
distance <- cor_test(Datos, "Ozone", "Solar.R", method = "distance"); distance

####################################### Bootstrap correlacion
library(boot)
Datos$x<-Wind
Datos$y<-Temp

Mi_corr<-cor.test(Wind, Temp)
str(Mi_corr)

Mi_corr$ estimate   #Aqui vemos el valor de r

set.seed(1)
b3 <- boot(Datos, 
           statistic = function(Datos, i) {
             cor(Datos[i, "x"], Datos[i, "y"], method='spearman')
           },
           R = 500
)
b3

boot.ci(b3, type = c("norm", "basic", "perc", "bca")) #bootstrapped CI.

plot(density(b3$t))
abline(v = Mi_corr$ estimate, lty = "dashed", col = "red") # Traemos el velor desde la corr

b3$t   # veamos todos los resutados del bootstrapped
hist(b3$t)
####################################### las Box PLots
# Basic boxplot
boxplot(Datos [1:4], col=terrain.colors(4) )

# install.packages("ggplot2")
library(ggplot2)

# Box plot
ggplot(Datos, aes(x = "", y = Temp)) + 
  geom_boxplot() +
  geom_jitter(colour = "blue")

library(ggbeeswarm)
# Beeswarm en ggplot2
ggplot(Datos, aes(x = "", y = Temp)) +
  geom_beeswarm(cex = 3)



# Box plot sobre el histograma

########## https://r-charts.com/es/distribucion/ggbeeswarm/
# Histograma
#      https://r-charts.com/es/distribucion/histograma-box-plot/
hist(Wind, prob = TRUE,
     col = "white",
     main = "")

# Nuevo gráfico
par(new = TRUE)

# Box plot
boxplot(Wind, horizontal = TRUE, axes = FALSE,
        col = rgb(0, 0.8, 1, alpha = 0.5))

# Caja
box()

###############
# install.packages("ggplot2")
library(ggplot2)

ggplot(Datos, aes(x = "", y = Temp, fill = "")) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.07) 

# https://r-charts.com/es/distribucion/grafico-violin-grupo-ggplot2/

# Correlacion parcial
# La correlación estudia la relación (lineal o monotónica) existente entre dos variables. 
# Puede ocurrir que la relación que muestran dos variables se deba a una tercera variable 
# que influye sobre las otras dos, a este fenómeno se le conoce como confounding.

# La correlación parcial permite estudiar la relación lineal entre dos variables bloqueando 
# el efecto de una tercera (o más) variables. Si el valor de correlación de dos variables 
# es distinto al valor de correlación parcial de esas mismas dos variables cuando se 
# controla una tercera, significa que la tercera variable influye en las otras dos.



Datos
library(MASS)
cor.test(x = Temp, y = Wind, method = "pearson")  #Correlacion

library(ppcor)
pcor.test(x = Temp, y = Wind, z =Month,
          method = "pearson")


############ TRANSFORMACION BOX COX
########## https://sixsigmastudyguide.com/box-cox-transformation/
shapiro.test(Datos$Temp) #  Prueba de normalidad si no es un dataframe
Lamba<-car::powerTransform(Temp, family="bcPower")  #Buscamos Lamba
str(Lamba)
Lamba$ start
Datos$Temp_Box<-Temp^Lamba$ start
Datos

shapiro.test(Datos$Temp_Box) #  Prueba de normalidad si no es un dataframe
