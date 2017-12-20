library(quantmod)
BKT <- getSymbols('BKT.MC', src = "yahoo", 
           from = "2012-01-01",
           to = "2017-01-01",
           auto.assign = FALSE)
df_precios_limits <- na.omit((BKT[,2]+BKT[,3]/2))

precio_compra1 <- 2.65

fechas_umbral_compra_limits1 <- NULL
for (i in (2:nrow(df_precios_limits))){
  contador <- i-1
  if (df_precios_limits[i,] < precio_compra1 && df_precios_limits[contador,] > precio_compra1 ){
    fechas_umbral_compra_limits1 <- c(fechas_umbral_compra_limits1,i)
  }
}

df_precios_limits[c(105,106,113,114,123,124,131,132),]


precio_compra2 <- 4

fechas_umbral_compra_limits2 <- NULL
for (j in (2:nrow(df_precios_limits))){
  contador2 <- j - 1
  if (df_precios_limits[j,] <= precio_compra2 && df_precios_limits[contador2,] > precio_compra2){
    fechas_umbral_compra_limits2 <- c(fechas_umbral_compra_limits2,j)
  }}

fechas_umbral_compra_limits2

precio_venta1 <- 6

fechas_umbral_venta_limits <- NULL
for (k in (2:nrow(df_precios_limits))){
  contador3 <- k-1
  if (df_precios_limits[k,] >= precio_venta1 && df_precios_limits[contador3,] < precio_venta1){
    fechas_umbral_venta_limits <- c(fechas_umbral_venta_limits,k)
  }}

fechas_umbral_venta_limits

df_precios_limits[c(439,440,447,448),]

resultado_neto_1 <- NULL
resultado_neto_2 <- NULL
resultado_ventas_1 <- NULL
resultado_ventas_2 <- NULL
total_compra_limits_1 <- NULL
total_compra_limits_2 <- NULL
resultado_ventas <- NULL
cantidad_compra_limits1 <- 20
cantidad_compra_limits2 <- 20
contador_1 <- 1
contador_compra_1 <- c(1)
contador_compra_2 <- c(1)
contador_while_1 <- 0
contador_while_2 <- 0
contador_2 <- 1
resultado <- NULL
precios_compra_limits2 <- NULL
importe_compra_limits2 <- NULL
cantidad_total_compra_limits2 <- NULL
precios_compra_limits1 <- NULL
importe_compra_limits1 <- NULL
cantidad_total_compra_limits1 <- NULL

for (i in (1:length(fechas_umbral_venta_limits))){
  while(fechas_umbral_venta_limits[i] > fechas_umbral_compra_limits1[contador_1] && 
        contador_1<length(fechas_umbral_compra_limits1+2)){
    for (j in (1:length(fechas_umbral_compra_limits1))){
      if(fechas_umbral_venta_limits[i] > fechas_umbral_compra_limits1[j]){
        precios_compra_limits1 <- c(precios_compra_limits1,df_precios_limits[fechas_umbral_compra_limits1[j],])
        cantidad_total_compra_limits1 <- c(cantidad_total_compra_limits1,cantidad_compra_limits1)
        importe_compra_limits1 <- c(importe_compra_limits1, precios_compra_limits1[j]*cantidad_compra_limits1)
        contador_1 <- contador_1 +1
      }
    }
    contador_while_1 <- contador_while_1+1
    contador_compra_1 <- c(contador_compra_1,length(importe_compra_limits1))
  }
  while(fechas_umbral_venta_limits[i] > fechas_umbral_compra_limits2[contador_2] && 
        contador_2<length(fechas_umbral_compra_limits2+2)){
    for (j in (1:length(fechas_umbral_compra_limits2))){
      if(fechas_umbral_venta_limits[i] > fechas_umbral_compra_limits2[j]){
        precios_compra_limits2 <- c(precios_compra_limits2,df_precios_limits[fechas_umbral_compra_limits2[j],])
        cantidad_total_compra_limits2 <- c(cantidad_total_compra_limits2,cantidad_compra_limits2)
        importe_compra_limits2 <- c(importe_compra_limits2, precios_compra_limits2[j]*cantidad_compra_limits2)
        contador_2 <- contador_2 +1
      }
    }
    contador_while_2 <- contador_while_2+1
    contador_compra_2 <- c(contador_compra_2,length(importe_compra_limits2))
  }
  
  if(length(contador_compra_1)>i){
    resultado_ventas_1 <- c(resultado_ventas_1,sum(cantidad_total_compra_limits1[contador_compra_1[i+1]-contador_compra_1[i]+1])* df_precios_limits[fechas_umbral_venta_limits[i],])
    total_compra_limits_1 <- c(total_compra_limits_1,sum(importe_compra_limits1[contador_compra_1[i+1]-contador_compra_1[i]+1]))
    resultado_neto_1 <- c(resultado_neto_1,resultado_ventas_1[i]-total_compra_limits_1[i])
  }
  if(length(contador_compra_1)>i){
    resultado_ventas_2 <- c(resultado_ventas_2,sum(cantidad_total_compra_limits2[contador_compra_2[i+1]-contador_compra_2[i]+1])* df_precios_limits[fechas_umbral_venta_limits[i],])
    total_compra_limits_2 <- c(total_compra_limits_2,sum(importe_compra_limits2[contador_compra_2[i+1]-contador_compra_2[i]+1]))
    resultado_neto_2 <- c(resultado_neto_2,resultado_ventas_2[i]-total_compra_limits_2[i])
  }
}

resultado_total <- sum(resultado_neto_1) + sum(resultado_neto_2)

#Cuando el usuario utilice el action button se hara un print del resultado obtenido con esta estrategia
if (!is.null(valores$limit)){
  print(paste0("You would gain/loss"," ",resultado,"€"))
}


