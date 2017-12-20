fechas_umbral_venta_limits
fechas_umbral_compra_limits1
fechas_umbral_compra_limits2

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
  
