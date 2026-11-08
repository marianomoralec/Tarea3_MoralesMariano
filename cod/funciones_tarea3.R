#Ejercicio 1: Alertas moviles
#Recibe: df (transacciones), ventana y k.alerta.
#Devuelve: data frame con fecha, n.tx, media, desv y alerta.
alerta.movil <- function(df, ventana, k.alerta){
  #Primero calculo la cantidad de transacciones por dia 
  diario <- df |> 
    group_by(fecha) |> 
    summarise(n.tx=n()) |> 
    ungroup() |> 
    arrange(fecha)
  #Ahora creo una columna vacia para las medias moviles
  diario$media <- NA_real_
  #Tambien creo la columna de la desviacion estandar
  diario$desv <- NA_real_
  #Ahora calculo la media movil y la desviacion estandar a cada uno
  if (nrow(diario) > ventana) {
    for (i in seq.int(ventana + 1, nrow(diario))) {
      anteriores <- diario$n.tx[(i-ventana):(i-1)]
      diario$media[i] <- mean(anteriores)
      diario$desv[i] <- sd(anteriores)
    }
  }
  #Creo la columna de alerta y la lleno con base a la condicion
  diario$alerta <- diario$n.tx > diario$media + k.alerta * diario$desv
  return(diario)
}

#Ejercicio 2: Ley de Benford
#Recibe: x, vector de montos de transacciones.
#Devuelve: MAD entre las proporciones observadas de los primeros digitos y la distribucion de Benford.

benford.mad<-function(x) {
  #Primero filtro los negativos ya que estoy trabajando con logaritmos y tambien los na
  x<-x[!is.na(x) & x > 0]
  #Reviso si quedan datos
  if (length(x) == 0) {
    return(NA_real_)
  }
  #Extraigo el primer digito
  digito <- floor(x / 10^floor(log10(x)))
  #Cuento cuantas veces sale cada uno
  frecuencia <- tabulate(digito, nbins = 9)
  #Calculo la proporcion
  proporcion <- frecuencia/sum(frecuencia)
  #Ahora calculo la proporcion de benford
  benford <- log10(1+1/(1:9))
  #Por ultimo calculo la mad
  mad <- mean(abs(proporcion-benford))
  return(mad)
}
#Ejercicio 2: Analisis de Benford por canton
#Recibe: df (transacciones), canal y tabla de cantones.
#Devuelve: data frame con codigo, canton, cantidad, MAD y clasificacion, ordenado de mayor a menor MAD.

benford.canton <- function(df, canal, cantones) {
  #filtro las transacciones del canal
  datos <- df[df$canal==canal, ]
  #Agrupo por canton y calculo la MAD
  resultado <- aggregate(
    monto_crc~cod_canton,
    data=datos,
    FUN=function(x) c(n=sum(!is.na(x) & x > 0), mad=benford.mad(x))
  )
  #Ahora lo pongo en el formato de un data frame
  resultado <- data.frame(
    cod_canton=resultado$cod_canton,
    n=resultado$monto_crc[, "n"],
    mad=resultado$monto_crc[, "mad"]
  )
  #Ahora agrego los nombres
  resultado <- merge(
    resultado,
    cantones[, c("cod_canton", "canton")],
    by="cod_canton"
  )
  #Clasifico la mad
  resultado$clase <- ifelse(
    resultado$mad <= 0.006, "conformidad cercana",
    ifelse(resultado$mad <= 0.012, "conformidad aceptable",
           ifelse(resultado$mad <= 0.015, "conformidad marginal", "no conformidad"))
  )
  #Los ordeno de mayor a menor mad
  resultado <- resultado[
    order(-resultado$mad),
    c("cod_canton", "canton", "n", "mad", "clase")
  ]
  #Elimino los nombres de las filas
  rownames(resultado) <- NULL

  return(resultado)
}