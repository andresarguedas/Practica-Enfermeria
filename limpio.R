##Limpieza de la base de clonazepam##===========================================
  
  # Primero, cargamos las librerías que ocupamos para este script:
  
  x <- c("memisc", "magrittr", "lubridate", "doParallel")
  lapply(x, library, character.only = T)
  remove(x)
  
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
  
  # El ID dado a los NA´s es el 70119, por lo que ahora vamos a ver el número de
  # paciente de los NA's y si para alguien con ese mismo número de paciente hay
  # un ID distinto, entonces se le asigna ese ID:
  
  v.na <- unique(data$paciente[is.na(data$id)])
  
  for(i in 1:length(unique(data$paciente[is.na(data$id)]))) {
    print(unique(data$ID[data$paciente == v.na[i]]))
  }
  
  for(k in 1:9) {
    data$ID[data$paciente == v.na[k]] <- max(data$ID) + 1
  }
  
  data$ID[data$paciente == v.na[10]] <- 99288
  
  for(l in 11:31) {
    data$ID[data$paciente == v.na[l]] <- max(data$ID) + 1
  }
  
  # Teniendo los ID's listos y los NA's revisados, procedemos a eliminar las 
  # variables restantes:
  
  data$identificacion <- NULL
  data$nombre <- NULL
  data <- data[c(18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)]
  
  # Necesitamos agregar la dosis y la frecuencia a la base:
  
  registerDoParallel(cores = 8)
  
  data[, 14:15] <- foreach::foreach(i = 1:nrow(data), .combine = rbind, .packages = c("stringr", "taRifx", "magrittr")) %dopar% {
    extraer.numeros(data$indicacion[i])
  }
  
  stopImplicitCluster()
  
  # Para empezar la limpieza de la base, ocupamos encontrar aquellos casos en 
  # los cuales el algoritmo presenta algún problema. En este caso, se va a 
  # seleccionar a aquellos donde la dosis o la frecuencia es 0, lo cual no puede
  # suceder:
  
  ceros <- which(clon$dosis == 0 | clon$frecuencia == 0)
  
  # El siguiente csv contiene las dosis y frecuencias corregidas para los casos 
  # presentados anteriormente:
  
  correccion.ceros <- read.csv2("corrections/clonazepam/correccion ceros.csv")
  
  # Con el archivo de correcciones cargado, vamos a reemplazar estos valores en 
  # la base:
  
  data[ceros, 14:15] <- correccion.ceros
  
  # Para mantener el orden, vamos a eliminar las variables anteriores:
  
  remove(ceros, correccion.ceros)
  
  # El siguiente paso es ver si hay algún NA en la base:
  
  which(is.na(data$dosis))
  data[174412, ]
  
  # Procedemos a cambiar el NA anterior por un 999, para poder omitirlo más 
  # facilmente de la limpieza posterior:
  
  data$dosis[174412] <- 999
  data$frecuencia[174412] <- 999
  
  # Ahora, vamos a ver si hay algún dato en el cual la dosis sea mayor a 1000, 
  # para evitar problemas con los NA's:
  
  which(data$dosis > 1000)
  data[197900, ]
  
  # Corregimos el caso anterior:
  
  data$dosis[197900] <- 60
  data$frecuencia[197900] <- 3
  
  # Por último, revisamos si hay alguna frecuencia que no sea entera:
  
  which(data$frecuencia%%1 != 0)
  data[10074, ]
  
  data$dosis[10074] <- 7
  data$frecuencia[10074] <- 1
  
  # El siguiente paso es agregar la variable de presentación a los datos que no 
  # lo tienen. En este caso, una presentación de 1 es para tabletas y una 
  # presentación de 2 es para cápsulas. Ya que ninguna de los casos en la base 
  # de gotas tiene presentación entonces vamos a ponerle presentación de 2 a 
  # todos aquellos que no tengan presentación:
  
  data$presentacion[which(data$presentacion == 0)] <- 2
  
  # Para hacer el análisis de casos extremos vamos a quitar los NA's, para que 
  # no generen ruido:
  
  data1 <- subset(data, data$dosis < 999)
  
  # Ya que la distribución de la dosis es distinta entre tabletas y gotas, vamos
  # a agregar la dosis en miligramos como una forma de estandarizar esta 
  # variable entre las dos presentaciones del medicamento, para poder hacer un 
  # análisis de casos extremos de forma conjunta:
  
  registerDoParallel(cores = 8)
  
  data1$mg <- foreach::foreach(i = 1:701834, .combine = rbind, .packages = c("stringr", "taRifx", "magrittr")) %dopar% {
    ifelse(data1$presentacion[i] == 1, 2 * data1$dosis[i], 0.125 * data1$dosis[i])
  }
  
  stopImplicitCluster()
  
  # Para la detección de casos extremos vamos a usar los hat-values con las 
  # variables de interés, dosis en miligramos y frecuencia:
  
  h <- hat(clon1[, c(15, 18)])
  
  # Ahora, vamos a tomar como casos extremos solo aquellos en los cuales el 
  # hat-value sea mayor a 10 veces la media de los hat-values, para poder 
  # trabajar con un número reducido de casos extremos dado el tamaño total de la
  # base de datos:
  
  v.extremos <- which(h > 10 * mean(h))
  
  # Cargamos el archivo csv con las correcciones en la dosis y la frecuencia de 
  # los casos extremos obtenidos anteriormente:
  
  correccion.extremos <- read.csv2("corrections/clonazepam/correccion casos extremos.csv")
  
  # Reemplazamos la dosis y la frecuencia de la base con la corregida:
  
  data1[v.extremos, 14:15] <- correccion.extremos
  
  # Ahora, para los casos corregidos anteriormente, vamos a volver a calcular la
  # dosis en miligramos apropiada:
  
  for(i in 1:length(v.extremos)) {
    data1$mg[v.extremos[i]] <- ifelse(data1$descripcion[v.extremos[i]] == "CLONAZEPAM 2 MG TABLETAS", 
                                      2 * data1$dosis[v.extremos[i]], 
                                      0.125 * data1$dosis[v.extremos[i]])
  }
  
  # De nuevo, para mantener el orden, borramos las variables usadas 
  # anteriormente:
  
  remove(v.extremos, correccion.extremos)
  
  # La dosis en miligramos actualmente no es una variable numérica, por lo que 
  # la ocupamos cambiar para que lo sea:
  
  data1$mg %<>% as.numeric()
  
  # Por último, juntamos la base corregida con los NA's que fueron excluidos al 
  # principio de la limpieza:
  
  data.limpio <- rbind(data1, data[which(data$dosis == 999), ])
  
  # Ahora, para facilitar el procesamiento de los NA's en análisis posteriores, 
  # cambiamos los 999 a NA's de verdad:
  
  data.limpio$dosis[which(data.limpio$dosis == 999)] <- NA
  data.limpio$frecuencia[which(data.limpio$frecuencia == 999)] <- NA
  
  # Ahora, guardamos la base completa y limpia:
  
  dir.create("F:/Proyecto enfermeria/Practica-Enfermeria/data")
  saveRDS(data.limpio, 
        file = "F:/Proyecto enfermeria/Practica-Enfermeria/data/clonazepam.rds")

##FIN##=========================================================================