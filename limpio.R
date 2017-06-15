## Script de limpieza===========================================================
  
  # Primero, cargamos el script con las funciones necesarias para la limpieza:
  
  source(paste(getwd(), "/funciones.R", sep = ""))
  
  # Cargamos las librerías necesarias:
  
  x <- c("lubridate")
  lapply(x, library, character.only = T)
  remove(x)
  
  # Ahora, cargamos el archivo .csv:
  
  datos <- read.csv2("F:/Psicotropicos/Clobazam 10mg. tabletas/Clobazam.csv")
  
  # Arreglamos los problemas en los tipos de variables:
  
  colnames(datos)[1] <- "Identificacion"
  options(scipen = 999)
  datos$Identificacion %<>% as.numeric()
  datos$Nombre %<>% as.character()
  datos$Fecha %<>% dmy()
  datos$Año <- year(datos$Fecha) 
  datos$Mes <- month(datos$Fecha)
  datos$Dia <- day(datos$Fecha)
  datos$Medico %<>% as.character()
  datos$Especialidad %<>% as.character()
  datos$Indicacion %<>% as.character()
  
  # Teniendo las variables arregladas, procedemos a asignar el ID único:
  
  datos$ID <- asignar.ID(datos$Identificacion, datos$Nombre)
  
  # Eliminamos el nombre y la identificación:
  
  datos$Identificacion <- NULL
  datos$Nombre <- NULL
  
  # Extraemos la dosis y la frecuencia:
  
  registerDoParallel(cores = 8)
  
  K <- ncol(datos)
  
  datos[, (K + 1):(K + 2)] <- foreach::foreach(i = 1:nrow(datos), .combine = rbind, .packages = c("stringr", "taRifx", "magrittr")) %dopar% {
    extraer.numeros(datos$Indicacion[i])
  }
  
  stopImplicitCluster()
  
  colnames(datos)[(K + 1):(K + 2)] <- c("Dosis", "Frecuencia")
  
  remove(K)
  
  # Procedemos al análisis de los mal clasificados, empezando por los ceros:
  
  ceros <- which(datos$dosis == 0 | datos$frecuencia == 0)
  
  # Vemos cuales son los que están mal clasificados:
  
  datos[ceros, ]
  
  # Correcciones (en caso necesario):
  
  remove(ceros)
  
  # Ahora, vemos si hay algún NA:
  
  nas <- which(is.na(datos))
  
  # Vemos cuales son los que tiene NA's:
  
  datos[nas, ]
  
  # Correcciones (en caso necesario):
  
  remove(nas)
  
  # Vemos si hay casos con dosis por encima de 999:
  
  nas.e <- which(datos$Dosis > 999)
  
  # vemos cuales son los que tienen dosis mayor a 999:
  
  datos[nas.e, ]
  
  # Correcciones (en caso necesario):
  
  remove(nas.e)
  
  # Vemos si hay frecuencias no enteras:
  
  f.frac <- which(datos$Frecuencia %% 1 != 0)
  
  # Vemos cuales son los que tienen frecuencia no entera:
  
  datos[f.frac, ]
  
  # Correcciones (en caso necesario):
  
  remove(f.frac)
  
  # Calculamos la dosis en mg's:
  
  datos$mg <- datos$Dosis * 10
  
  # Ahora, vamos a ver si hay casos extremos:
  
  h <- hat(cbind(datos$mg, datos$Frecuencia))
  v.extremos <- which(h > 10 * mean(h))
  datos[v.extremos, ]
  
  # Correcciones (en caso necesario):
  
  remove(h, v.extremos)
  
  # Limpiamos las especialidades:
  
  datos$Especialidad %<>% limpiar.especialidad()
  
  # Guardamos los datos:
  
  dir.create(paste(getwd(), "/Clobazam", "/data", sep = ""))
  saveRDS(datos, 
          file = paste(getwd(), "/Clobazam","/data", "/Clobazam", ".rds", sep = ""))
  
##FIN===========================================================================