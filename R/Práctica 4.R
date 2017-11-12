### Programación R. Práctica 4
### Ejercicio 1
str(swiss)
head(swiss)
x <- swiss$Education[1:25]
sort(x,method="sh",index.return=TRUE) 
#index.return nos indica la posición que tienen los elementos en la lista previamente a ser ordenados
y <- as.integer(rnorm(200,5,7))
sort(y,method="quick")
z <- c('Carlos','Antonio','Paco','Luis')
### El método shellshort se utiliza también para texto mientras que el método quick solo vale para numeros
sort(z,method="sh",index.return=TRUE)
### Ordenación parcial
sort(x,partial=1:5)

###Ejercicio 2
x <- as.integer(rnorm(100,42,(2.5**(1/2))))
x
y <- as.integer(rnorm(100,177,(10**(1/2))))
y
## a) Meter las variables en un dataframe denominado datos
datos <- data.frame(x,y)
head(datos)
## b) Calcular la media y la varianza de x e y mediante la función apply
calculomedia <- apply(datos,2,mean)
calculomedia
calculovarianza <-apply(datos,2,var)
calculovarianza
## c) Determinar la covarianza y la correlación, representar en un gráfico
covarianza <- cov(datos$x,datos$y,method="pearson")
covarianza
correlación <- cor(datos$x,datos$y, method="pearson")
correlación
msg <- sprintf("Covariance = %.2f Correlation = %.2f", covarianza,correlación)
msg
plot(datos$x,datos$y,main=msg,
     xlab = "talla de pie",
     ylab = "estatura (cm)")
##d) creación de una regresión linear e incorporarla en el plot
regresion <- lm(y~x)
abline(regresion)

a = list()
str(a)
a <- c(a,rpois(9,3))
str(a)
### Ejercicio 3
k1 <- as.integer(readline(prompt="Dime la dimensión K1: "))
k2 <- as.integer(readline(prompt="Dime la dimensión K2: "))
lambdas <- as.integer(readline(prompt="Dime el parámetro lambda: "))
x <- rpois((k1*k2),lambdas)
mat <- matrix(x,nrow = k1,ncol = k2)
mat2 <- mat

#complentando por filas
  for(i in 1:k1){
    for(j in 1:k2){
      mat2[i,j]<- x[k2*(i-1)+j]
    }
  }
mat2

#Completando por columnas
mat3<-mat
for(j in 1:k2){
  for(i in 1:k1){
    mat3[i,j]<- x[k1*(j-1)+i]
  }
}
mat3

#Calcular la transpuesta.
mattrans<- matrix(0, k2, k1)   ##matriz llena de ceros.
for(i in 1:k2){
  for (j in 1:k1){
    mattrans[i,j]<- mat3[j,i]
  }
}
mattrans

