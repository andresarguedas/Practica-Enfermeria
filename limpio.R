##Limpieza de la base de clonazepam##===========================================
  
  # Primero, cargamos las librerías que ocupamos para este script:
  
  library(memisc)
  library(magrittr)
  library(lubridate)
  
  # Ahora, sourceamos las funciones necesarias para la limpieza de la base:
  
  source('F:/Proyecto enfermeria/Practica-Enfermeria/funciones.R')
  
  # Cargamos el archivo con las tabletas:
  
  tabletas <- as.data.set(spss.system.file(
    'F:/practica enfermeria/I semestre/CLONAZEPANRE.sav'))
  
  # Arreglamos las variables de las bases para su uso más fácil:
  
  tabletas$id %<>% as.numeric()
  tabletas$identificacion %<>% as.numeric()
  tabletas$nombre %<>% as.character()
  tabletas$paciente %<>% as.numeric()
  tabletas$sexo %<>% as.factor()
  tabletas$unidad.ejecutora %<>% as.numeric()
  tabletas$edad %<>% as.numeric()
  tabletas$producto %<>% as.character()
  tabletas$descripcion %<>% as.factor()
  tabletas$fecha %<>% dmy()
  tabletas$fecha %<>% day()
  tabletas$mes %<>% as.numeric()
  tabletas$año %<>% as.numeric()
  tabletas$medico %<>% as.character()
  tabletas$especialidad %<>% as.character()
  tabletas$indicacion %<>% as.character()
  tabletas$dosis %<>% as.numeric()
  tabletas$frecuencia %<>% as.numeric()
  tabletas$presentacion %<>% as.factor()
  tabletas$edadrec %<>% as.numeric()
  tabletas$especificacion %<>% as.factor()
  tabletas$mg %<>% as.numeric()
  tabletas %<>% as.data.frame()
  
  # Ahora, cargamos la base con las gotas:
  
  gotas <- as.data.set(spss.system.file(
    "F:/practica enfermeria/Clonazepán gotas/Clonagotas(1).sav"))
  
  # Y hacemos lo mismo que con las tabletas:
  
  gotas$v1 %<>% as.numeric()
  options(scipen = 999)
  gotas$identificacion %<>% as.numeric()
  gotas$unidad %<>% as.numeric()
  gotas$nombre %<>% as.character()
  gotas$sexoc %<>% as.factor()
  gotas$edad %<>% as.numeric()
  gotas$producto %<>% as.character()
  gotas$descripcion %<>% as.character()
  gotas$fecha %<>% dmy()
  gotas$fecha %<>% day()
  gotas$mes %<>% as.numeric()
  gotas$anno %<>% as.numeric()
  gotas$cantidad %<>% as.numeric()
  gotas$medico %<>% as.character()
  gotas$especialidad %<>% as.character()
  gotas$indicacion %<>% as.character()
  gotas$indic0 %<>% as.character()
  gotas %<>% as.data.frame()
  
  # Eliminamos las variables de la base de tabletas y gotas que no ocupamos y 
  # creamos las variables nuevas que ocupamos:
  
  tabletas$id <- NULL
  tabletas$edadrec <- NULL
  gotas$v1 <- NULL
  gotas$cantidad <- NULL
  gotas$indic0 <- NULL
  gotas$dosis <- 0
  gotas$frecuencia <- 0
  gotas$presentacion <- 0
  gotas$especificacion <- 0
  gotas$mg <- 0
  gotas$paciente <- 0
  
  # Reordenamos las variables en la base de gotas y les cambiamos el nombre para 
  # que sean iguales a las variables en la base de tabletas:  
  
  gotas <- gotas[c(1, 3, 19, 4, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
                   17, 18)]
  names(gotas)[c(4, 5, 11)] <- names(tabletas)[c(4, 5, 11)]
  cbind(names(gotas), names(tabletas))
  names(gotas) == names(tabletas)
  
  # Ahora, juntamos ambas bases en una sola base:
  
  options(scipen = 999)
  data <- rbind(gotas, tabletas)
  data$nombre %<>% as.character()
  data$fecha %<>% as.numeric()
  data$descripcion %<>% as.character()
  data$medico %<>% as.character()
  data$especialidad %<>% as.character()
  data$indicacion %<>% as.character()
  data$presentacion %<>% as.factor()
  data$especificacion %<>% as.factor()
  
  # Aplicamos la siguiente función para crear un ID único para cada cédula y 
  # eliminamos las variables de nombre y cédula:
  
  data$ID <- asignar.ID(data$identificacion, data$nombre, na.especial = F)
  
  ## APARTADO PARA CORREGIR NA'S##
  
  data$identificacion <- NULL
  data$nombre <- NULL
  data <- data[c(18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)]
  
  # Ahora, guardamos la base completa y limpia:
  
  dir.create("F:/Proyecto enfermeria/Practica-Enfermeria/data")
  saveRDS(data, 
        file = "F:/Proyecto enfermeria/Practica-Enfermeria/data/clonazepam.rds")

##FIN##=========================================================================