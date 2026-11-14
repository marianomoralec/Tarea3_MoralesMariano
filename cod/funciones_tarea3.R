library(tidyverse)
#Ejercicio 1: Alertas móviles
#Recibe: df (transacciones), ventana y k.alerta.
#Devuelve: fecha, n.tx, media, desv y alerta.
alerta.movil <- function(df, ventana, k.alerta) {
  #Cuento las transacciones por día
  diario <- df |>
    count(fecha, name="n.tx") |>
    arrange(fecha)
  #Creo las columnas para la media y desviación móvil
  diario <- diario |>
    mutate(
      media=NA_real_,
      desv=NA_real_
    )
  
  #Calculo usando los días anteriores
  if(nrow(diario)>ventana) {
    for (i in seq.int(ventana+1, nrow(diario))) {
      anteriores <- diario$n.tx[(i-ventana):(i-1)]
      diario$media[i] <- mean(anteriores, na.rm=T)
      diario$desv[i] <- sd(anteriores, na.rm=T)
    }
  }
  #Identifico los días que presentan una alerta
  diario <- diario |>
    mutate(
      alerta=n.tx > media+k.alerta*desv
    )
  return(diario)
}


#Ejercicio 2: Ley de Benford
#Recibe: vector de montos.
#Devuelve: MAD de los primeros dígitos.
benford.mad <- function(x) {
  #Elimino valores faltantes y no positivos
  x <- x[!is.na(x) & x>0]
  #Verifico que existan observaciones
  if(length(x)==0) {
    return(NA_real_)
  }
  #Extraigo el primer dígito
  digito <- floor(x/10^floor(log10(x)))
  #Cuento la frecuencia de los dígitos del 1 al 9
  frecuencia <- tabulate(digito, nbins=9)
  #Calculo las proporciones observadas
  proporcion <- frecuencia/sum(frecuencia)
  #Calculo las proporciones teóricas de Benford
  benford <- log10(1+1/(1:9))
  #Calculo la desviación absoluta media
  mad <- mean(abs(proporcion-benford))
  return(mad)
}


#Ejercicio 2: Análisis de Benford por cantón
#Recibe: df, canal y tabla de cantones.
# Devuelve: código, cantón, n, MAD y clasificación.

benford.canton <- function(df, canal, cantones) {
  #Filtro las transacciones del canal seleccionado
  datos <- df |>
    filter(canal==.env$canal)
  #Agrupo por cantón y calculo cantidad y MAD
  resultado <- datos |>
    summarise(
      n=sum(!is.na(monto_crc) & monto_crc>0),
      mad=benford.mad(monto_crc),
      .by=cod_canton
    )
  #Agrego el nombre de cada cantón
  resultado <- resultado |>
    left_join(
      cantones |>
        select(cod_canton, canton),
      by="cod_canton"
    )
  #Clasifico según el valor del MAD
  resultado <- resultado |>
    mutate(
      clase=case_when(
        mad<=0.006 ~ "conformidad cercana",
        mad<=0.012 ~ "conformidad aceptable",
        mad<=0.015 ~ "conformidad marginal",
        .default="no conformidad"
      )
    )
  #Ordeno de mayor a menor MAD
  resultado <- resultado |>
    arrange(desc(mad)) |>
    select(cod_canton, canton, n, mad, clase)
  return(resultado)
}

#Ejercicio 3: Valor acumulado e intereses
#Calcula el valor acumulado de las transacciones
#al 31 de diciembre, con depósitos al final de cada mes.
valor.acumulado <- function(df, tasa.anual, canal) {
  #Calculo la tasa mensual equivalente
  tasa.mensual <- (1+tasa.anual)^(1/12)-1
  #Filtro el canal y obtengo el mes de cada transacción
  datos <- df |>
    filter(canal==.env$canal) |>
    mutate(
      mes=lubridate::month(as.Date(fecha))
    )
  #Sumo los montos por mes
  montos.mensuales <- datos |>
    summarise(
      monto=sum(monto_crc, na.rm=TRUE),
      .by=mes
    ) |>
    tidyr::complete(
      mes=1:12,
      fill=list(monto=0)
    )
  #Calculo el factor de acumulación
  montos.mensuales <- montos.mensuales |>
    mutate(
      factor=(1+tasa.mensual)^(12-mes)
    )
  #Calculo el valor acumulado
  va <- sum(montos.mensuales$monto*montos.mensuales$factor)
  #Calculo el total depositado
  total.depositado <- sum(montos.mensuales$monto)
  # Calculo los intereses ganados
  interes <- va-total.depositado
  #Devuelvo los resultados
  return(list(
    tabla=montos.mensuales,
    tasa.mensual=tasa.mensual,
    va=va,
    interes=interes
  ))
}