####FUNDAMENTOS DE CÁLCULO


### Derivadas

## Cálculo de la derivada de X^2 y evaluación en distintos puntos-

g = mosaicCalc::D(x^2 ~ x )


plotFun(g,xlim = range(0,10)) #dibuja el gráfico incluyendo rango

h= makeFun(2*x^2-5*x+2 ~ x) #para valorar funciones en un punto

h(x=2)
h(x=5)

plotFun(h(x)~x,xlim = range(-5,5))

j= mosaicCalc::D(x^3-x ~ x)
j
j(1)
j(3)

plotFun(j(x)~x, xlim = range(-5,5))

# OTRA FORMA DE DERIVAR

A = symbolicD((x^3-x) ~x)
A
B = symbolicD(x^4 ~x)
B

## segundas derivadas

j = mosaicCalc::D(x^3 - x ~x) #primera derivada
j

dj = mosaicCalc::D(j(x) ~x) #segunda derivada
dj

### Funciones de varias variables

l = mosaicCalc::D(4*x^2*sin(y) ~ x)
l

n = mosaicCalc::D(4*x^2*sin(y) ~ y)
n

dl = mosaicCalc::D(4*x^2*sin(y) ~ x + x) #segundas derivadas
dl

dn = mosaicCalc::D(4*x^2*sin(y) ~ y + y)
dn

dln = mosaicCalc::D(4*x^2*sin(y) ~ x + y)
dln 

#Derivada del producto. Pag 20.

b = mosaicCalc::D((x^3-x)*(5*x^4+x^2)~ x)
b

#Derivada del cociente
e = mosaicCalc::D(1/(x-2) ~x)
e

#Algunos ejemplos pag. 24
k = mosaicCalc::D((x^3+x^2)^50 ~ x)
k

L = mosaicCalc::D(((x-1)/(x+3))^(1/3) ~ x)
L

#### Optimización

library(nloptr)

f = function(x) x*sin(4*x) #no hay que poner ninngún paréntesis ni corchetes detrás de function (x)
curve(f,0,3)

optimize(f,c(0,3),lower=0,upper=3) #ofrece el primer mínimo
optimize(f,c(0,3),lower=2,upper=3) #ofrece el mínimo global
optimize(f,c(0,3),lower=0,upper=3, maximum = T) #ofrece el máximo

## Estudiar la concavidad y convexidad

ff = mosaicCalc::D(x*sin(4*x) ~x) #primera derivada
ff
dff = mosaicCalc::D(ff(x) ~x) #segunda derivada
dff

curve(dff,c(0,10))

###

g = function(x) x*(20-2*x)*(16-2*x)
curve(g,0,10)

optimize(g,c(0,10),lower=0, upper= 10, maximum = T)
optimize(g,c(0,10),lower=0, upper= 10)

#Estudiar la concavidad y convexidad:

gg= mosaicCalc::D(x*(20-2*x)*(16-2*x) ~ x) #primera derivada
gg

dgg= mosaicCalc::D(gg(x) ~ x) #segunda derivada
curve(dgg,c(-10,10))

##Estudiar la concavidad/convexidad pag.59
h= function(x)




