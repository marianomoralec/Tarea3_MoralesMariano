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