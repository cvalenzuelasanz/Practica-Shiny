#Generación de una muestra aleatoria de una distribución bivariante a partir de una distribución normal multivariante.


#install.packages(MBESS)
library(MBESS)
library(ggplot2)
library(matrixcalc)
library(MASS)

#Definimos el tamaño muestra, correlación, vector de desviaciones típicas y vector de medias.

n <- 700
correlacion <- 1
desviacion_tipica <- c(0.5,1)
media_variables <- c(2,4)

#Definimos la matriz de correlación en función de la correlación establecida

matriz_corr <- matrix(c(1,correlacion,correlacion,1),nrow =2)

#Calculo de la matriz de covarianzas a partir de la matriz de correlación. 
#Tenemos que incluir el vector de desviaciones típicas
matriz_cov <- cor2cov(matriz_corr,desviacion_tipica)

#Funcón que genera muestra aleatorias a partir de un vector de medias y matriz de covarianzas.
muestra <- data.frame(mvrnorm(n,media_variables,matriz_cov))
colnames(muestra, do.NULL = FALSE)
colnames(muestra) <- c("X","Y")
#Representación gráfica

ggplot(muestra, aes(x= X,y = Y)) +
    geom_point()


