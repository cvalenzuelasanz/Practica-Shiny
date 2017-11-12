#Matrices

X <- c(1,2,3,4)

A <- matrix(c(seq(1,6,1)),nrow=2,ncol=3,byrow=F)
B <- matrix(c(seq(1,6,1)),2,3,byrow=T)
A
B

#Calculos
A+B

A-B

A*2

t(A)
t(B)
t(X)

dim(t(B))

dim(t(x))
matrix(x)
dim(matrix(x))

A*B #multiplica elemento por elemento

#multiplicación de matrices

Q%%W

A%*%B
B%*%A

#juntando matrices

A
C <- matrix(c(1,2,3,4,5,6),2,byrow = F)
C

cbind(A,C) #junta columnas
rbind(A,C) #junta filas

## Diagonales y trazas:

D <- matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow = 3)
D

diag(D)
sum(diag(D))


## Determinantes
det(D)



## Inversas

E <- matrix(c(2,3,3,4),2,2,byrow=T)
E
E;solve(E) # calcula la inversa para matrices no singulares

# las matrices cuyo determinante sea cero, no se pueden invertir.


#Diagonalización de matrices

G <- matrix(c(1,4,9,1),2,2,byrow = T)
G
eigen(G) #Proporciona los autovalores y autovectores asociados.

#Matriz de transición

PE <- matrix(c(0.5,0.5,0.25,0.75),2,2,byrow=T)
PE

auto <- eigen(PE)

autovectors <- auto$vectors

#Matriz H de autovectores obtenidos

AV <- matrix(autovectors,2,2,byrow=F)
AV

#Calculo de la inversa

AVI <- matrix(c(-0.4714045,-0.942809,-0.7453569, 0.745356),2,2,byrow=T)

#Matriz de los autovalores

J <- matrix(c(1,0,0,0.25),2,2,byrow=T)
J

#Cálculo de Pn = HJ^nH^(-1) donde en J^n hacemos n=100

AV%*%J^100%*%AVI

#El proceso se estabiliza en DP=1/3 y RV=2/3.







