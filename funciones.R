##Funciones para limpieza y procesamiento de la base##==========================
  
  # Mediante esto cargamos las libraries requeridas para las funciones.
  
  libraries <- c("stringr", "taRifx", "magrittr", "stringdist", "doParallel", 
         "lubridate", "ggplot2", "cowplot", "gridExtra", "grid", "scales")
  lapply(libraries, require, character.only = T)
  remove(libraries)
  
  # Esta función identifica las fracciones en un vector de caracteres (en la 
  # forma numero/numero) y las transforma en un número:
  
  fraccion.a.numero <- function(x) {
    temp <- strsplit(x, split = "/")
    for(i in 1:length(temp)) {
      y <- temp[i]
      y %<>% unlist() %>%
             destring()
      if(length(y) == 2) {
        temp[i] <- y[1] / y[2]
      } else {
        temp[i] <- y
      }
    }
    return(unlist(destring(temp)))
  }
  
  # Esta función transforma los números escritos en un string a dígitos, para 
  # poder ser procesados más fácilmente:
  
  texto.a.numero <- function(x) {
    x %<>% tolower() %>%
           gsub("un octavo", "1/8", .) %>%
           gsub("una y media", "3/2", .) %>%
           gsub("dos y media", "5/2", .) %>%
           gsub("media", "1/2", .) %>%
           gsub("un cuarto", "1/4", .) %>%
           gsub("tres cuartos", "3/4", .) %>%
           gsub("una y tres cuartos", "7/4", .) %>%
           gsub("treinta y cinco", "35", .) %>%
           gsub("dos y medio", "5/2", .)
    numeros <- list(cero = 0, uno = 1, una = 1, dos = 2, tres = 3, 
                    cuatro = 4, cinco = 5, seis = 6, siete = 7, ocho = 8, 
                    nueve = 9, diez = 10, once = 11, doce = 12, trece = 13, 
                    catorce = 14, quince = 15, dieciseis = 16, diecisiete = 17, 
                    dieciocho = 18, diecinueve = 19, veinte = 20, 
                    ventiuno = 21, ventiun = 21, ventidos = 22, 
                    ventitres = 23, venticuatro = 24, venticinco = 25, 
                    ventiseis = 26, ventisiete = 27, ventiocho = 28, 
                    ventinueve = 29,
                    veintiuno = 21, veintiun = 21, veintidos = 22, 
                    veintitres = 23, veinticuatro = 24, veinticinco = 25, 
                    veintiseis = 26, veintisiete = 27, veintiocho = 28, 
                    veintinueve = 29, treinta = 30, cuarenta = 40,
                    cincuenta = 50, sesenta = 60, setenta = 70)
    wsplit <- unlist(strsplit(x, " "))
    for(i in 1:length(wsplit)) {
      if(wsplit[i] %in% names(numeros)) {
        wsplit[i] %<>% gsub(., as.character(numeros[.]), .)
      }
    }
    x <- paste0(wsplit, collapse = " ")
    return(x)
  }
  
  # Esta función extrae todos los números, en forma numérica, que encuentra en 
  # un vector de caracteres y devuelve un vector numérico con los valores. Aquí 
  # falta hacer una función que transforme números escritos con letras en 
  # digitos y una forma de lograr discriminar que números se deben escoger 
  # basados en la palabra que le sigue, es decir, no tomar en cuenta el 3 en 
  # "durante 3 meses".
  
  extraer.numeros <- function(x) {
    if(is.character(x) == F) {
      stop("El argumento ocupa ser un vector de caracteres")
    }
    if(x == "") {
      return(c(0, 0))
    } else {
    d <- 0
    f <- 0
    x %<>% gsub("tableta", "", ., ignore.case = T) %>%
           gsub("media veces", " ", ., ignore.case = T) %>%
           gsub("  ", " ", .) %>%
           gsub("bid", " 2 veces y ", .) %>%
           gsub("tid", " 3 veces y ", .) %>%
           gsub("[.]\\(?[0-9/]+\\)?", "", .) %>%
           gsub("\\s*\\([^\\)]+\\)", "", .) %>%
           gsub(" I ", " Y ", .) %>%
           texto.a.numero() %>%
           gsub("\\(?[0-9/]+\\)?[/]\\(?[0-9/]+\\)?[/]\\(?[0-9/]+\\)?", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? [/]\\(?[0-9/]+\\)?[/]\\(?[0-9/]+\\)?", " ", .) %>%
           gsub("[(] \\(?[0-9/]+\\)? [)]", " ", .) %>%
           gsub("[/]\\(?[0-9/]+\\)?[/]$", " ", .) %>%
           gsub("[/]\\(?[0-9/]+\\)?[/]", " ", .) %>%
           gsub("[/]\\(?[0-9/]+\\)?[/][.]$", " ", .) %>%
           gsub("[/][/].*", " ", .) %>%
           gsub("[/][*].*", " ", .) %>%
           gsub("[**].*", " ", .) %>%
           gsub("[+][+].*", " ", .) %>%
           gsub("[.][.].*", " ", .) %>%
           gsub("[-][-].*", " ", .) %>%
           gsub("[<].*", " ", .) %>%
           gsub(" [/] ", " ", .) %>%
           gsub("am,md", " 2 veces y ", .) %>%
           gsub("am y md", " 2 veces y ", .) %>%
           gsub("c-\\(?[0-9/]+\\)?", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? mes.*", " ", .) %>%
           gsub("\\(?[0-9/]+\\)?mes.*", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? semana", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? semanas", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? dias", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? dia", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? noches", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? de la noche", " ", .) %>%
           gsub("inicia.*", " ", .) %>%
           gsub("hasta.*", " ", .) %>%
           gsub("llev.*", " ", .) %>%
           gsub("retiro.*", " ", .) %>%
           gsub("pendiente.*", " ", .) %>%
           gsub("ajusta.*", " ", .) %>%
           gsub("ajuste.*", " ", .) %>%
           gsub("completar.*", " ", .) %>%
           gsub("se despacha.*", " ", .) %>%
           gsub("receta.*", " ", .) %>%
           gsub("del.*", " ", .) %>%
           gsub("fecha.*", " ", .) %>%
           gsub("hace \\(?[0-9/]+\\)?", " ", .) %>%
           gsub("[0-9]$", "", .) %>%
           gsub("horas\\(?[0-9/]+\\)?", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? min", " ", .) %>%
           gsub("x \\(?[0-9/]+\\)? d", " ", .) %>%
           gsub("por \\(?[0-9/]+\\)? d", " ", .) %>%
           gsub("por \\(?[0-9/]+\\)?.", " ", .) %>%
           gsub("por \\(?[0-9/]+\\)? h", " ", .) %>%
           gsub("\\(?[0-9/]+\\)?d", " ", .) %>%
           gsub("# \\(?[0-9/]+\\)?", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? d$", " ", .) %>%
           gsub("\\(?[0-9/]+\\)?:\\(?[0-9/]+\\)?", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? :\\(?[0-9/]+\\)?", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? p.m.", " ", .) %>%
           gsub("m4", " ", .) %>%
           gsub("\\(?[0-9/]+\\)?p.m.", " ", .) %>%
           gsub("[(]\\(?[0-9/]+\\)?am[)]", " ", .) %>%
           gsub("[(]\\(?[0-9/]+\\)?pm[)]", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? de la tarde", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? cc", " ", .) %>%
           gsub("\\(?[0-9/]+\\)? cucharada", " ", .) %>%
           gsub(" las \\(?[0-9/]+\\)?", " ", .) %>%
           gsub(" el \\(?[0-9/]+\\)?", " ", .) %>%
           gsub("1 vez", " ", ., fixed = T) %>%
           gsub(".y", " y", ., fixed = T) %>%
           gsub("  ", " ", ., fixed = T)
    for(i in 1:length(strsplit(x, split = " y ")[[1]])) {
      y <- unlist(strsplit(x, split = " y "))[i]
      if(y != x & length(grep("caso n", y)) > 0) {
        d1 <- 0
        f1 <- 0
      } else {
        k <- 1
        if(length(grep("c/\\(?[0-9/]+\\)?", y)) > 0) {
          k <- 24 / as.numeric(str_extract_all(str_extract_all(y, "c/\\(?[0-9]+\\)?")[[1]], 
                                               "\\(?[0-9]+\\)?")[[1]])
          y %<>% gsub("c/\\(?[0-9/]+\\)?", " ", .)
        }
        if(length(grep("cada \\(?[0-9/]+\\)? horas", y)) > 0) {
          k <- 24 / as.numeric(str_extract_all(str_extract_all(y, "cada \\(?[0-9/]+\\)? horas")[[1]], 
                                               "\\(?[0-9/]+\\)?")[[1]])
          y %<>% gsub("cada \\(?[0-9/]+\\)? horas", " ", .)
        }
        if(length(grep("\\(?[0-9/]+\\)? vec", y)) > 0) {
          k <- as.numeric(str_extract_all(str_extract_all(y, "\\(?[0-9/]+\\)? vec")[[1]], 
                                          "\\(?[0-9/]+\\)?")[[1]])
          y %<>% gsub("\\(?[0-9/]+\\)? vec", " ", .)
        }
        if(length(grep("/", y)) != 0) {
          aa <- strsplit(y, "")[[1]]
          v1 <- which(strsplit(y, "")[[1]] == "/")
          y1 <- vector(length = length(v1))
          for(i in 1:length(v1)) {
            ifelse(v1[i] == 1 | v1[i] == nchar(y), y1[i] <- FALSE, 
                     y1[i] <- aa[v1[i] - 1] %in% paste(0:9) & aa[v1[i] + 1] %in% paste(0:9))
            }
          aa[v1[!y1]] <- " "
          y <- paste(aa, collapse = "")
        }
        y %<>% gsub("\\(?[0-9/]+\\)?h ", " ", .) %>%
               gsub("\\(?[0-9/]+\\)?h[)]", " ", .)
        num <- str_extract_all(y, "\\(?[0-9/]+\\)?")[[1]]
        if(length(grep("/", num)) != 0) {
          num %<>% fraccion.a.numero()
        }
        nums <- destring(num, keep = "0-9./")
        d1 <- sum(nums)
        f1 <- length(nums)
        if(k > 1) {
          f1 <- f1 * k
          d1 <- d1 * f1
        }
      }
      d <- d + d1
      f <- f + f1
    }
    return(c(d, f))
    }
  }
  
  # Mediante esta función se puede limpiar el vector de nombres, dado que hay
  # ciertas palabras que no brindan nada de información y solo generan ruido.
  
  limpieza.nombres <- function(x) {
    x %<>% gsub(" NO.*OTRO", "", .) %>%
           str_replace_all(., "[^[:graph:]]", " ") %>%
           gsub(" NO.*DICA", "", .) %>%
           gsub("NO$", "", .) %>%
           gsub(" NIO", "", .) %>%
           gsub("N I O", "", .) %>%
           gsub("N.I.O", "", .) %>%
           gsub(" NI$", "", .) %>%
           gsub("(NO.*OTRO)", "", .) %>%
           gsub("NOINDIC", "", .) %>%
           gsub("INDEFINIDO", "", .) %>%
           gsub("U.AP", "", .) %>%
           gsub("U.A.P.", "", .) %>%
           gsub("U AP", "", .) %>%
           gsub("DEL SO", "", .) %>%
           gsub("DEL S", "", .) %>%
           gsub("DE LOS S", "", .) %>%
           gsub("DE LA TRIN", "", .) %>%
           gsub("DE LA TRI", "", .) %>%
           gsub("DE LA TR", "", .) %>%
           gsub("DE LA T", "", .) %>%
           gsub("DE LOS ANGE", "", .) %>%
           gsub("DE LOS ANG", "", .) %>%
           gsub("DE LOS AN", "", .) %>%
           gsub("DE LOS A", "", .) %>%
           gsub("DEL LOS ANG", "", .) %>%
           gsub("DE ANGE", "", .) %>%
           gsub("DE SAN G", "", .) %>%
           gsub("DE SAN M", "", .) %>%
           gsub("DE SAN A", "", .) %>%
           gsub("DE SAN B", "", .) %>%
           gsub("DE SAN", "", .) %>%
           gsub("DE SA", "", .) %>%
           gsub("DE GUADA", "", .) %>%
           gsub("DE GUAD", "", .) %>%
           gsub("DE GUA", "", .) %>%
           gsub("DE LAS PIEDA", "", .) %>%
           gsub("DE LAS PIE", "", .) %>%
           gsub("DE LAS P", "", .) %>%
           gsub("DEL PIL ", "", .) %>%
           gsub("DEL PI ", "", .) %>%
           gsub("DE LAS ME", "", .) %>%
           gsub("DE LAS M", "", .) %>%
           gsub("DE LA CAN", "", .) %>%
           gsub("DE LA CO", "", .) %>%
           gsub("DE LA C", "", .) %>%
           gsub("DE CARME", "", .) %>%
           gsub("DEL C", "", .) %>%
           gsub("DEL ROSARIO", "", .) %>%
           gsub("DEL ROSARI", "", .) %>%
           gsub("DEL ROSAR", "", .) %>%
           gsub("DEL ROSA", "", .) %>%
           gsub("DEL ROS", "", .) %>%
           gsub("DEL RO", "", .) %>%
           gsub("DEL R", "", .) %>%
           gsub("DEL ESPI", "", .) %>%
           gsub("DE FATIMA", "", .) %>%
           gsub("DE FAT", "", .) %>%
           gsub("DE F", "", .) %>%
           gsub("DEL GER", "", .) %>%
           gsub("DE GER", "", .) %>%
           gsub("DE GE", "", .) %>%
           gsub("DEL MIL", "", .) %>%
           gsub("DE LOS DOLOR", "", .) %>%
           gsub("DE LOS DO", "", .) %>%
           gsub("DE LOS DESA", "", .) %>%
           gsub("DE LOS SANTOS", "", .) %>%
           gsub("DE JE", "", .) %>%
           gsub("DE J", "", .) %>%
           gsub("DE PIEDA", "", .) %>%
           gsub("DE P", "", .) %>%
           gsub("DEL N", "", .) %>%
           gsub("DEL AUX", "", .) %>%
           gsub("DEL BUSTO", "", .) %>%
           gsub("DE LOS", "", .) %>%
           gsub("DE LAS", "", .) %>%
           gsub(" DEL$", "", .) %>%
           gsub(" DE LO$", "", .) %>%
           gsub("DE LA O", "DELAO", .) %>%
           gsub("DE L", "", .) %>%
           gsub(" D$", "", .) %>%
           gsub(" RA$", "", .) %>%
           gsub(" A$", "", .) %>%
           gsub(" R$", "", .) %>%
           gsub(" MI$", "", .) %>%
           gsub(" C$", "", .) %>%
           gsub(" AN$", "", .) %>%
           gsub(" G$", "", .) %>%
           gsub(" B$", "", .) %>%
           gsub(" DE S$", "", .) %>%
           gsub(" DEL M$", "", .) %>%
           gsub(" DE M$", "", .) %>%
           gsub(" DE DO$", "", .) %>%
           gsub(" AR$", "", .) %>%
           gsub(" DEL PI$", "", .) %>%
           gsub(" DE CL$", "", .) %>%
           gsub(" DE CL ", " ", .) %>%
           gsub(" O ", " ", .) %>%
           gsub(" AR ", " ", .) %>%
           gsub(" DE DO ", " ", .) %>%
           gsub(" DEL M ", " ", .) %>%
           gsub(" DE M ", " ", .) %>%
           gsub(" DE S ", " ", .) %>%
           gsub(" B ", " ", .) %>%
           gsub(" G ", " ", .) %>%
           gsub(" AN ", " ", .) %>%
           gsub(" C ", " ", .) %>%
           gsub(" RA ", " ", .) %>%
           gsub(" A ", " ", .) %>%
           gsub(" D ", " ", .) %>%
           gsub(" R ", " ", .) %>%
           gsub(" MI ", " ", .) %>%
           gsub("^A ", "", .) %>%
           gsub("DELAO", "DE LA O", .) %>%
           gsub("\\s+$", "", .) %>%
           gsub("  ", " ", .)
    return(x)
  }
  
  # Esta función sirve para limpiar las especialidades médicas:
  
  limpiar.especialidad <- function(x) {
    x %<>% gsub("MEDICINA", "", .) %>%
           gsub("FONIATRIAA", "AUDIOLOGIA", .) %>%
           gsub("PSIQUIAT$", "PSIQUIATRIA", .) %>%
           gsub("NEUROLOG$", "NEUROLOGIA", .) %>%
           gsub("PSICOSOMAT$", "PSICOSOMATICA", .) %>%
           gsub("PSICOSOMATI$", "PSICOSOMATICA", .) %>%
           gsub("INFECTOLOGI$", "INFECTOLOGIA", .) %>%
           gsub("GASTROENTER$", "GASTROENTEROLOGIA", .) %>%
           gsub("NEUROCARDI$", "NEUROCARDIO", .) %>%
           gsub("GERIATRI$", "GERIATRIA", .) %>%
           gsub("CARDIOLO$", "CARDIOLOGIA", .) %>%
           gsub("REHABILITAC$", "REHABILITACION", .) %>%
           gsub("REUMATOLOGI$", "REUMATOLOGIA", .) %>%
           gsub("DERMATOLOGI$", "DERMATOLOGIA", .) %>%
           gsub("ENDOCRINOLO$", "ENDOCRINOLOGIA", .) %>%
           gsub("ADOLESCENTE$", "ADOLESCENTES", .) %>%
           gsub("EMERGENC$", "EMERGENCIA", .) %>%
           gsub("FA$", "FAMILIAR", .) %>%
           gsub("UCI MEDICOS", "", .) %>%
           gsub("UCI", "", .) %>%
           gsub("PENSION", "", .) %>%
           gsub("(PSIQUIATRIA).*", "\\1", .) %>%
           gsub("(PEDIATRIA).*", "\\1", .) %>%
           gsub("(PSICOLOGIA).*", "\\1", .) %>%
           gsub("(CIRUGIA).*", "\\1", .) %>%
           gsub("(ODONTOLOGIA).*", "\\1", .) %>%
           gsub("(RADIOLOGIA).*", "\\1", .) %>%
           gsub("(GINECO).*", "\\1", .) %>%
           gsub("(ONCOLOGIA).*", "\\1", .) %>%
           gsub("(NUTRICION).*", "\\1", .) %>%
           gsub("(NEUROCARDIO).*", "\\1", .) %>%
           gsub("(FAMILIAR).*", "\\1", .) %>%
           gsub("(CLINICA).*", "\\1", .) %>%
           gsub("(AUDIOLOGIA).*", "\\1", .) %>%
           gsub("NO APLICA", "", .) %>%
           gsub("^\\s+|\\s+$", "", .)
    return(x)
  }
  
  # Esta función está solo para ahorrar tiempo y no tener que escribir todo 
  # muchas veces.
  
  u <- function(x) {
    return(unlist(strsplit(x, " ", fixed = T)))
  }
  
  # Esta función calcula la distancia entre dos nombres y devuelve TRUE si los 
  # dos nombres son iguales y FALSE si no lo son.
  
  f <- function(x, y) {
    x1 <- u(x)
    y1 <- u(y)
    d <- stringdistmatrix(x1, y1)
    aj <- which(d == 0, arr.ind = T)
    daj <- which(dist(aj) == 1)
    while(length(daj) > 0) {
      b <- aj[combn(1:nrow(aj), 2)[, daj[1]][2], ]
      d[b[1], b[2]] <- 2
      aj <- which(d == 0, arr.ind = T)
      daj <- which(dist(aj) == 1)
    }
    if(nrow(d) >= ncol(d)) {
      a <- apply(d, 2, min)
    } else {
      a <- apply(d, 1, min)
    }
    return(all(a <= 1))
  }
  
  # Esta función crea grupos entre nombres que le son suministrados, poniendo 
  # nombres iguales en el mismo grupo.
  
  w <- function(x) {
    y <- x
    n <- length(x)
    q <- seq(1:n)
    p <- vector(length = n)
    k <- 1
    while(n > 1) {
      v <- foreach(i = 2:n, .combine = c, .packages = "stringdist", .export = c("f", "u")) %dopar% {
        f(y[1], y[i])
      }
      h <- which(v == T) + 1
      if(length(h) > 0) {
        p[q[c(1, h)]] <- k
      } else {
        p[q[1]] <- k
      }
      k <- k + 1
      q <- q[-c(1, h)]
      y <- y[-c(1, h)]
      n <- n - length(h) - 1
    }
    if(n == 1) {
      p[q[1]] <- k
    }
    return(p)
  }
  
  # Esta función asigna un ID numérico único por cada elemento único de un 
  # cierto vector. Aquí el argumento "x" sería la cedula y el argumento "y" el 
  # nombre. La función regresa un vector con los IDs.
  
  asignar.ID <- function(x, y) {
    registerDoParallel(cores = 8)
    ID <- match(x, unique(x))
    uni <- unique(ID)
    for(i in 1:length(uni)) {
      wh <- which(ID == uni[i])
      IDt <- max(ID)
      if(length(unique(y[wh])) > 1) {
        a <- w(y[wh])
        for(j in 1:length(a)) {
          ID[wh[j]] <- IDt + a[j]
        }
      }
    }
    uni1 <- unique(y)
    for(l in 1:length(uni1)) {
      wh1 <- unique(ID[which(y == uni1[l])])
      if(length(wh1) > 1) {
        c1 <- min(wh1)
        for(k in 1:length(wh1)) {
          ID[which(ID == wh1[k])] <- c1
        }
      }
    }
    stopImplicitCluster()
    return(ID)
  }
  
  # Esta función calcula la dosis y la frecuencia y hace alguna limpieza de los
  # datos. 
  
  extraccion <- function(x, medicamento) {
    colnames(x)[1] <- "Identificacion"
    options(scipen = 999)
    x$Identificacion %<>% as.numeric()
    x$Nombre %<>% as.character()
    x$Fecha %<>% dmy()
    x$Ano <- year(x$Fecha) 
    x$Mes <- month(x$Fecha)
    x$Dia <- day(x$Fecha)
    x$Medico %<>% as.character()
    x$Especialidad %<>% as.character()
    x$Indicacion %<>% as.character()
    x$Especialidad %<>% limpiar.especialidad()
    x$ID <- asignar.ID(x$Identificacion, x$Nombre)
    x$Identificacion <- NULL
    x$Nombre <- NULL
    registerDoParallel(cores = 8)
    K <- ncol(x)
    x[, (K + 1):(K + 2)] <- foreach::foreach(i = 1:nrow(x), .combine = rbind, 
                                             .packages = c("stringr", "taRifx", 
                                                           "magrittr"), 
                                             .export = c("extraer.numeros", 
                                                         "texto.a.numero", 
                                                         "fraccion.a.numero")) %dopar% {
      extraer.numeros(x$Indicacion[i])
                                             }
    stopImplicitCluster()
    colnames(x)[(K + 1):(K + 2)] <- c("Dosis", "Frecuencia")
    remove(K)
    return(x)
  }
  
  correccion <- function(x, medicamento) {
    ceros <- which(x$dosis == 0 | x$frecuencia == 0)
    if(length(ceros > 0)) {
      x[ceros, c(11, 17, 18)] %<>% edit()
    }
    remove(ceros)
    nas <- which(is.na(x))
    if(length(nas > 0)) {
      x[nas, c(11, 17, 18)] %<>% edit()
    }
    remove(nas)
    nas.e <- which(x$Dosis > 999)
    if(length(nas.e > 0)) {
      x[nas.e, c(11, 17, 18)] %<>% edit()
    }
    remove(nas.e)
    f.frac <- which(x$Frecuencia %% 1 != 0)
    if(length(f.frac > 0)) {
      x[f.frac, c(11, 17, 18)] %<>% edit()
    }
    remove(f.frac)
    x$mg <- x$Dosis * as.numeric(unlist(regmatches(x$Descripcion,
                                                   gregexpr("[[:digit:]]+\\.*[[:digit:]]*", 
                                                            x$Descripcion))))
    h <- hat(cbind(x$mg, x$Frecuencia))
    v.extremos <- which(h > 10 * mean(h))
    if(length(v.extremos > 0)) {
      x[v.extremos, c(11, 17, 18)] %<>% edit()
    }
    remove(h, v.extremos)
    x$Dosis[which(x$Dosis == 999)] <- NA
    x$Frecuencia[which(x$Frecuencia == 999)] <- NA
    return(x)
  }
  
  # Esta función sirve para cargar datos de forma más fácil:
  
  cargar <- function(medicamento) {
    m.validos <- c("clobazam", "clonazepam", "diazepan", "fenobarbital", 
                   "lorazepan", "midazolan", "tiopental")
    me <- tolower(medicamento)
    if(!me %in% m.validos) {
      stop(paste(me, " no es un medicamento válido.", sep = ""))
    }
    ME <- paste(toupper(substr(me, 1, 1)), substr(me, 2, nchar(me)), sep = "")
    x <- readRDS(paste(getwd(), "/", ME, "/data", "/", ME, ".rds", sep = ""))
    return(x)
  }
  
  # Esta función crea todos los gráficos necesarios con su respectivo título y
  # fuente y los guarda en una carpeta:
  
  graficos <- function(medicamento) {
    x <- cargar(medicamento)
    x <- na.omit(x)
    me <- tolower(medicamento)
    ME <- paste(toupper(substr(me, 1, 1)), substr(me, 2, nchar(me)), sep = "")
    ml <- list(clobazam = 20, clonazepam = 8, midazolan = 15, lorazepan = 2.5)
    DDD <- as.numeric(ml[me])
    message("Creando y guardando los gráficos...")
    pb <- txtProgressBar(max = 10, style = 3)
    dir.create(paste(getwd(), "/", ME, "/plots", sep = ""), showWarnings = F)
    x$edad.rec <- cut(x$Edad, breaks = c(-Inf, 10, 15, 20, 25, 30, 35, 40, 45, 
                                         50, 55, 60, 65, 70, 75, 80, 85, 90, 
                                         Inf), 
                      labels = c("0 a 10", "11 a 15", "16 a 20", "21 a 25", 
                                 "26 a 30", "31 a 35", "36 a 40", "41 a 45", 
                                 "46 a 50", "51 a 55", "56 a 60", "61 a 65", 
                                 "66 a 70", "71 a 75", "76 a 80", "81 a 85", 
                                 "86 a 90", "Más de 91"))
    x.na <- na.omit(x)
    theme1 <- theme(axis.text.x = element_text(size = 11 * 0.8), 
                    axis.text.y = element_text(size = 11 * 0.8), 
                    axis.title.x = element_text(size = 11), 
                    axis.title.y = element_text(size = 11), 
                    plot.title = element_text(size = 14, face = "bold", 
                                              hjust = 0.5))
    fuente1 <- "Fuente: Datos de consumo de psicotrópicos de la CCSS del 2011 al 2015"
    i <- 0
    mujeres <- sum(x.na$Sexo == 'F') / nrow(x.na)
    hombres <- sum(x.na$Sexo == 'M') / nrow(x.na)
    df1 <- data.frame(
      sexo = c("Hombres", "Mujeres"),
      porcentaje = c(hombres, mujeres)
    )
    bp <- ggplot(df1, aes(x = "", y = porcentaje, fill = sexo)) + 
          geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y", start = 0)
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
      )
    i <- i + 1
    pie <- pie + 
           scale_fill_manual(values = c("brown2", "blue3"), name = "Sexo") +  
           blank_theme +
           theme(axis.text.x = element_blank(), 
                 plot.title = element_text(hjust = 0.5)) +
           geom_text(aes(y = c(cumsum(porcentaje)[-length(porcentaje)] + .5,
                               cumsum(porcentaje)[-length(porcentaje)]), 
                    label = percent(porcentaje)), size = 5) + 
           ggtitle(paste("Figura ", i, 
                         ":\nPorcentaje del total de prescripciones de ", me, " por sexo,\n en Costa Rica, entre el 2011 y el 2015", 
                         sep = ""))
    g1 <- arrangeGrob(pie, bottom = textGrob(fuente1, x = 0, hjust = -0.1, 
                                             vjust = -0.1, 
                                             gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots",
                                           "/1 Pastel.png", sep = ""),
              g1))
    setTxtProgressBar(pb, i)
    x.na$Sexo %<>% as.numeric()
    x.paciente <- aggregate(x = x.na[, c(1, 2, 19)], by = list(ID = x.na$ID), 
                            FUN = function(x) c(m = min(x), l = length(x), 
                                                sum(x > DDD)))
    x.na$Sexo %<>% factor()
    x.paciente$Sexo <- x.paciente$Sexo[, 1]
    x.paciente$Prescripciones <- x.paciente$Edad[, 2]
    x.paciente$Edad <- x.paciente$Edad[, 1]
    x.paciente$mg <- x.paciente$mg[, 3]
    x.paciente$edad.rec <- cut(x.paciente$Edad, breaks = c(-Inf, 10, 15, 20, 25,
                                                           30, 35, 40, 45, 50, 
                                                           55, 60, 65, 70, 75,
                                                           80, 85, 90, Inf), 
                               labels = c("0 a 10", "11 a 15", "16 a 20", 
                                          "21 a 25", "26 a 30", "31 a 35", 
                                          "36 a 40", "41 a 45", "46 a 50", 
                                          "51 a 55", "56 a 60", "61 a 65", 
                                          "66 a 70", "71 a 75", "76 a 80", 
                                          "81 a 85", "86 a 90", "Más de 91"))
    x.ddd <- subset(x.paciente, mg >= 1)
    x.edad <- aggregate(x.na$mg, by = list(Edad = x.na$edad.rec),
                        FUN = function(x) c(length(x), mean(x), sum(x > DDD)))
    x.edad$promedio <- x.edad$x[, 2]
    x.edad$ddd <- x.edad$x[, 3]
    x.edad$x <- x.edad$x[, 1]
    x.edad$ddd.p <- x.edad$ddd / x.edad$x * 100
    i <- i + 1
    barras <- ggplot(data = x.edad, aes(x = Edad, y = x)) + 
      geom_col(fill = "brown") + 
      ylab("Prescripciones") +
      ggtitle(paste("Figura ", i, 
                    ":\nCantidad de prescripciones de ", me, " por grupos\n de edad, en Costa Rica, entre el 2011 y el 2015", 
                    sep = "")) +
      theme1 +
      theme(axis.text.x = element_text(angle = 70, vjust = 0.5))
    g2 <- arrangeGrob(barras, bottom = textGrob(fuente1, x = 0, hjust = -0.1, 
                                                vjust = -0.1, 
                                                gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots", 
                            "/2 Histograma prescripciones.png", 
                            sep = ""),
           g2))
    setTxtProgressBar(pb, i)
    x.sexoedad <- aggregate(x = x.na, by = list(Edad = x.na$edad.rec, 
                                                Sexo = x.na$Sexo), 
                            FUN = length)[, c(1, 2, 18)]
    x.sexoedad$ID <- ifelse(x.sexoedad$Sexo == '1', 
                            x.sexoedad$ID / length(which(x.na$Sexo == '1')) * 100, 
                            x.sexoedad$ID / length(which(x.na$Sexo == '2')) * 100)
    x.sexoedad$ID <- ifelse(x.sexoedad$Sexo == '1', 
                            -1 * x.sexoedad$ID, 
                            x.sexoedad$ID)
    colnames(x.sexoedad)[3] <- "Prescripciones"
    i <- i + 1
    m1 <- ceiling(max(abs(x.sexoedad$Prescripciones)) / 2.5) * 2.5
    m2 <- floor(m1 / 5) * 5
    piramide <- ggplot(data = x.sexoedad, aes(x = Edad, y = Prescripciones, 
                                       fill = factor(Sexo))) + 
      geom_bar(data = subset(x.sexoedad, Sexo == '1'), stat = "identity") +
      geom_bar(data = subset(x.sexoedad, Sexo == '2'), stat = "identity") +
      coord_flip() +
      theme1 +
      theme(panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank()) +
      scale_fill_manual(values = c("brown2", "blue3"), 
                        labels = c("Hombres", "Mujeres"),
                        name = "Sexo") +
      scale_y_continuous(limits = c(-m1, m1), breaks = seq(-m2, m2, 5), 
                         labels = as.character(abs(seq(-m2, m2, 5)))) +
      xlab("Edad") +
      ylab("Porcentaje") +
      ggtitle(paste("Figura ", i, 
                    ":\nPorcentaje del total de prescripciones de ", me, " por grupos\n de edad y sexo, en Costa Rica, entre el 2011 y el 2015", 
                    sep = ""))
    g3 <- arrangeGrob(piramide, bottom = textGrob(fuente1, x = 0, hjust = -0.1, 
                                                  vjust = -0.1, 
                                                  gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots", 
                            "/3 Piramide.png", 
                            sep = ""),
           g3))
    setTxtProgressBar(pb, i)
    i <- i + 1
    media.edad <- ggplot(data = x.edad, aes(x = Edad, y = promedio, group = 1)) + 
      geom_point(size = 2.5) + 
      geom_line() + 
      ylab("Dosis promedio (en mg)") +
      ggtitle(paste("Figura ", i, 
                    ":\nDosis diaria promedio de ", me, ", en miligramos, por grupos de edad,\n en Costa Rica, entre el 2011 y el 2015", 
                    sep = "")) +
      theme1 +
      theme(axis.text.x = element_text(angle = 70, vjust = 0.5))
    g4 <- arrangeGrob(media.edad, bottom = textGrob(fuente1, x = 0, 
                                                    hjust = -0.1, vjust = -0.1, 
                                                    gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots", 
                            "/4 Dosis promedio.png", 
                            sep = ""),
           g4))
    setTxtProgressBar(pb, i)
    x.tiempo <- aggregate(x = x.na[, 1], by = list(Mes = x.na$Mes, 
                                                   Ano = x.na$Ano), FUN = length)
    x.tiempo$tiempo <- ymd(paste(x.tiempo$Ano, x.tiempo$Mes, 1))
    i <- i + 1
    serie.tiempo <- ggplot(data = x.tiempo, aes(x = tiempo, y = x)) + 
      geom_line() + 
      ylab("Cantidad de prescripciones") + 
      xlab("Año") + 
      theme1 +
      scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
      ggtitle(paste("Figura ", i, 
                    ":\nCantidad de prescripciones de ", me, " por mes y año,\n en Costa Rica, entre el 2011 y el 2015", 
                    sep = ""))
    
    g5 <- arrangeGrob(serie.tiempo, bottom = textGrob(fuente1, x = 0, 
                                                      hjust = -0.1, 
                                                      vjust = -0.1, 
                                                      gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots", 
                            "/5 Serie de tiempo.png", 
                            sep = ""),
           g5))
    setTxtProgressBar(pb, i)
    i <- i + 1
    p.ano <- ggplot(data = x.tiempo, aes(x = Ano, y = x)) + 
      geom_col(fill = "brown") + 
      ylab("Cantidad de prescripciones") +
      theme1 +
      ggtitle(paste("Figura ", i, 
                    ":\nCantidad de prescripciones de ", me, " por año,\n en Costa Rica, entre el 2011 y el 2015", 
                    sep = ""))
    g6 <- arrangeGrob(p.ano, bottom = textGrob(fuente1, x = 0, hjust = -0.1, 
                                                vjust = -0.1, 
                                                gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots", 
                            "/6 Prescripciones anuales.png", 
                            sep = ""),
           g6))
    setTxtProgressBar(pb, i)
    i <- i + 1
    barras.ddd <- ggplot(data = x.ddd, aes(x = edad.rec, y = mg)) +
      geom_col(fill = "brown") + 
      ylab("Prescripciones") +
      xlab("Edad") +
      ggtitle(paste("Figura ", i, 
                    ":\nCantidad de prescripciones de ", me, " por encima de la DDD\n por grupos de edad, en Costa Rica, entre el 2011 y el 2015", 
                    sep = "")) +
      theme1 +
      theme(axis.text.x = element_text(angle = 70, vjust = 0.5))
    g7 <- arrangeGrob(barras.ddd, bottom = textGrob(fuente1, x = 0, 
                                                    hjust = -0.1, vjust = -0.1, 
                                                    gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots", 
                            "/7 DDD por edad.png", 
                            sep = ""),
           g7))
    setTxtProgressBar(pb, i)
    i <- i + 1
    dispersion <- ggplot(data = x.ddd, aes(x = x.ddd$mg, y = x.ddd$Prescripciones)) +
      geom_point() +
      geom_jitter() +
      geom_abline(slope = 1, intercept = 0, colour = 2) +
      theme1 +
      xlab("Cantidad de prescripciones mayores a la DDD") +
      ylab("Cantidad de prescripciones") +
      ggtitle(paste("Figura ", i, 
                    ":\nCantidad de prescripciones totales y mayores a la DDD de ", me, "\n por paciente, en Costa Rica, entre el 2011 y el 2015\n(La línea roja representa que la totalidad de \nprescripciones son mayores a la DDD)", 
                    sep = ""))
    g8 <- arrangeGrob(dispersion, bottom = textGrob(fuente1, x = 0, 
                                                    hjust = -0.1, vjust = -0.1, 
                                                    gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots", 
                            "/8 Dispersion DDD.png", 
                            sep = ""),
           g8))
    setTxtProgressBar(pb, i)
    x.medico <- aggregate(x = x.na$mg, by = list(Medico = x.na$Medico), FUN = function(x) c(length(x), sum(x > DDD)))
    x.medico$ddd <- x.medico$x[, 2]
    x.medico$x <- x.medico$x[, 1]
    u1 <- unique(x.medico$x)
    u1 <- u1[order(u1)]
    v1 <- matrix(nrow = length(u1), ncol = 2)
    for(l in 1:length(u1)) {
      v1[l, 1] <- sum(x.medico$x >= u1[l]) / nrow(x.medico) * 100
      v1[l, 2] <- sum(x.medico$x[x.medico$x >= u1[l]]) / nrow(x.na) * 100
    }
    v1 %<>% as.data.frame()
    i <- i + 1
    medicos.por <- ggplot(data = v1, aes(x = V1, y = V2)) + 
      geom_line() +
      xlab("Porcentaje de médicos") +
      ylab("Porcentaje de prescripciones emitidas") +
      theme1 +
      ggtitle(paste("Figura ", i, 
                    ":\nPorcentaje de prescripciones de ", me, " emitidas por un cierto\n porcentaje de médicos, en Costa Rica, entre el 2011 y el 2015", 
                    sep = ""))
    g9 <- arrangeGrob(medicos.por, bottom = textGrob(fuente1, x = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots", 
                            "/9 Medicos DDD.png", 
                            sep = ""),
           g9))
    setTxtProgressBar(pb, i)
    i <- i + 1
    dispersion.m <- ggplot(data = x.medico, aes(x = x.medico$ddd, y = x.medico$x)) +
      geom_point() +
      geom_jitter() +
      theme1 +
      xlab("Cantidad de prescripciones mayores a la DDD") +
      ylab("Cantidad de prescripciones") +
      geom_abline(slope = 1, intercept = 0, colour = 2) +
      ggtitle(paste("Figura ", i, 
                    ":\nCantidad de prescripciones total y mayores a la DDD recetadas\n según médico, en Costa Rica, entre el 2011 y el 2017\n(La línea roja representa que la totalidad de \nprescripciones son mayores a la DDD)", 
                    sep = ""))
    g10 <- arrangeGrob(dispersion.m, bottom = textGrob(fuente1, x = 0, 
                                                       hjust = -0.1, 
                                                       vjust = -0.1, 
                                                       gp = gpar(fontsize = 10)))
    suppressMessages(ggsave(filename = paste(getwd(), "/", ME, "/plots", 
                            "/10 Dispersion medicos.png", 
                            sep = ""),
           g10))
    setTxtProgressBar(pb, i)
    close(pb)
  }
  
##FIN##=========================================================================