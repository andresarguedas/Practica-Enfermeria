##Funciones para limpieza y procesamiento de la base##==========================
  
  # Mediante esto cargamos las libraries requeridas para las funciones.
  
  x <- c("stringr", "taRifx", "magrittr", "stringdist", "doParallel")
  lapply(x, require, character.only = T)
  remove(x)
  
  # Esta función identifica las fracciones en un vector de caracteres (en la 
  # forma numero/numero) y las transforma en un número:
  
  fraccion.a.numero <- function(x) {
    temp <- strsplit(x, split = "/")
    for(i in 1:length(temp)) {
      y <- temp[i]
      y %<>% unlist()
      y %<>% destring()
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
    x %<>% tolower()
    x %<>% gsub("un octavo", "1/8", .)
    x %<>% gsub("una y media", "3/2", .)
    x %<>% gsub("dos y media", "5/2", .)
    x %<>% gsub("media", "1/2", .)
    x %<>% gsub("un cuarto", "1/4", .)
    x %<>% gsub("tres cuartos", "3/4", .)
    x %<>% gsub("una y tres cuartos", "7/4", .)
    x %<>% gsub("treinta y cinco", "35", .)
    x %<>% gsub("dos y medio", "5/2", .)
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
    x %<>% gsub("tableta", "", ., ignore.case = T)
    x %<>% gsub("media veces", " ", ., ignore.case = T)
    x %<>% gsub("  ", " ", .)
    x %<>% gsub("bid", " 2 veces y ", .)
    x %<>% gsub("tid", " 3 veces y ", .)
    x %<>% gsub("[.]\\(?[0-9/]+\\)?", "", .)
    x %<>% gsub("\\s*\\([^\\)]+\\)", "", .)
    x %<>% gsub(" I ", " Y ", .)
    x %<>% texto.a.numero()
    x %<>% gsub("\\(?[0-9/]+\\)?[/]\\(?[0-9/]+\\)?[/]\\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? [/]\\(?[0-9/]+\\)?[/]\\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub("[(] \\(?[0-9/]+\\)? [)]", " ", .)
    x %<>% gsub("[/]\\(?[0-9/]+\\)?[/]$", " ", .)
    x %<>% gsub("[/]\\(?[0-9/]+\\)?[/]", " ", .)
    x %<>% gsub("[/]\\(?[0-9/]+\\)?[/][.]$", " ", .)
    x %<>% gsub("[/][/].*", " ", .)
    x %<>% gsub("[/][*].*", " ", .)
    x %<>% gsub("[**].*", " ", .)
    x %<>% gsub("[+][+].*", " ", .)
    x %<>% gsub("[.][.].*", " ", .)
    x %<>% gsub("[-][-].*", " ", .)
    x %<>% gsub("[<].*", " ", .)
    x %<>% gsub(" [/] ", " ", .)
    x %<>% gsub("am,md", " 2 veces y ", .)
    x %<>% gsub("am y md", " 2 veces y ", .)
    x %<>% gsub("c-\\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? mes.*", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)?mes.*", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? semana", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? semanas", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? dias", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? dia", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? noches", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? de la noche", " ", .)
    x %<>% gsub("inicia.*", " ", .)
    x %<>% gsub("hasta.*", " ", .)
    x %<>% gsub("llev.*", " ", .)
    x %<>% gsub("retiro.*", " ", .)
    x %<>% gsub("pendiente.*", " ", .)
    x %<>% gsub("ajusta.*", " ", .)
    x %<>% gsub("ajuste.*", " ", .)
    x %<>% gsub("completar.*", " ", .)
    x %<>% gsub("receta.*", " ", .)
    x %<>% gsub("del.*", " ", .)
    x %<>% gsub("fecha.*", " ", .)
    x %<>% gsub("hace \\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub("[0-9]$", "", .)
    x %<>% gsub("horas\\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? min", " ", .)
    x %<>% gsub("x \\(?[0-9/]+\\)? d", " ", .)
    x %<>% gsub("por \\(?[0-9/]+\\)? d", " ", .)
    x %<>% gsub("por \\(?[0-9/]+\\)?.", " ", .)
    x %<>% gsub("por \\(?[0-9/]+\\)? h", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)?d", " ", .)
    x %<>% gsub("# \\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? d$", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)?:\\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? :\\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? p.m.", " ", .)
    x %<>% gsub("m4", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)?p.m.", " ", .)
    x %<>% gsub("[(]\\(?[0-9/]+\\)?am[)]", " ", .)
    x %<>% gsub("[(]\\(?[0-9/]+\\)?pm[)]", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? de la tarde", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? cc", " ", .)
    x %<>% gsub("\\(?[0-9/]+\\)? cucharada", " ", .)
    x %<>% gsub(" las \\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub(" el \\(?[0-9/]+\\)?", " ", .)
    x %<>% gsub("1 vez", " ", ., fixed = T)
    x %<>% gsub(".y", " y", ., fixed = T)
    x %<>% gsub("  ", " ", ., fixed = T)
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
        y %<>% gsub("\\(?[0-9/]+\\)?h ", " ", .)
        y %<>% gsub("\\(?[0-9/]+\\)?h[)]", " ", .)
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
    x %<>% gsub(" NO.*OTRO", "", .)
    x %<>% str_replace_all(., "[^[:graph:]]", " ")
    x %<>% gsub(" NO.*DICA", "", .)
    x %<>% gsub("NO$", "", .)
    x %<>% gsub(" NIO", "", .)
    x %<>% gsub("N I O", "", .)
    x %<>% gsub("N.I.O", "", .)
    x %<>% gsub(" NI$", "", .)
    x %<>% gsub("(NO.*OTRO)", "", .)
    x %<>% gsub("NOINDIC", "", .)
    x %<>% gsub("INDEFINIDO", "", .)
    x %<>% gsub("U.AP", "", .)
    x %<>% gsub("U.A.P.", "", .)
    x %<>% gsub("U AP", "", .)
    x %<>% gsub("DEL SO", "", .)
    x %<>% gsub("DEL S", "", .)
    x %<>% gsub("DE LOS S", "", .)
    x %<>% gsub("DE LA TRIN", "", .)
    x %<>% gsub("DE LA TRI", "", .)
    x %<>% gsub("DE LA TR", "", .)
    x %<>% gsub("DE LA T", "", .)
    x %<>% gsub("DE LOS ANGE", "", .)
    x %<>% gsub("DE LOS ANG", "", .)
    x %<>% gsub("DE LOS AN", "", .)
    x %<>% gsub("DE LOS A", "", .)
    x %<>% gsub("DEL LOS ANG", "", .)
    x %<>% gsub("DE ANGE", "", .)
    x %<>% gsub("DE SAN G", "", .)
    x %<>% gsub("DE SAN M", "", .)
    x %<>% gsub("DE SAN A", "", .)
    x %<>% gsub("DE SAN B", "", .)
    x %<>% gsub("DE SAN", "", .)
    x %<>% gsub("DE SA", "", .)
    x %<>% gsub("DE GUADA", "", .)
    x %<>% gsub("DE GUAD", "", .)
    x %<>% gsub("DE GUA", "", .)
    x %<>% gsub("DE LAS PIEDA", "", .)
    x %<>% gsub("DE LAS PIE", "", .)
    x %<>% gsub("DE LAS P", "", .)
    x %<>% gsub("DEL PIL ", "", .)
    x %<>% gsub("DEL PI ", "", .)
    x %<>% gsub("DE LAS ME", "", .)
    x %<>% gsub("DE LAS M", "", .)
    x %<>% gsub("DE LA CAN", "", .)
    x %<>% gsub("DE LA CO", "", .)
    x %<>% gsub("DE LA C", "", .)
    x %<>% gsub("DE CARME", "", .)
    x %<>% gsub("DEL C", "", .)
    x %<>% gsub("DEL ROSARIO", "", .)
    x %<>% gsub("DEL ROSARI", "", .)
    x %<>% gsub("DEL ROSAR", "", .)
    x %<>% gsub("DEL ROSA", "", .)
    x %<>% gsub("DEL ROS", "", .)
    x %<>% gsub("DEL RO", "", .)
    x %<>% gsub("DEL R", "", .)
    x %<>% gsub("DEL ESPI", "", .)
    x %<>% gsub("DE FATIMA", "", .)
    x %<>% gsub("DE FAT", "", .)
    x %<>% gsub("DE F", "", .)
    x %<>% gsub("DEL GER", "", .)
    x %<>% gsub("DE GER", "", .)
    x %<>% gsub("DE GE", "", .)
    x %<>% gsub("DEL MIL", "", .)
    x %<>% gsub("DE LOS DOLOR", "", .)
    x %<>% gsub("DE LOS DO", "", .)
    x %<>% gsub("DE LOS DESA", "", .)
    x %<>% gsub("DE LOS SANTOS", "", .)
    x %<>% gsub("DE JE", "", .)
    x %<>% gsub("DE J", "", .)
    x %<>% gsub("DE PIEDA", "", .)
    x %<>% gsub("DE P", "", .)
    x %<>% gsub("DEL N", "", .)
    x %<>% gsub("DEL AUX", "", .)
    x %<>% gsub("DEL BUSTO", "", .)
    x %<>% gsub("DE LOS", "", .)
    x %<>% gsub("DE LAS", "", .)
    x %<>% gsub(" DEL$", "", .)
    x %<>% gsub(" DE LO$", "", .)
    x %<>% gsub("DE LA O", "DELAO", .)
    x %<>% gsub("DE L", "", .)
    x %<>% gsub(" D$", "", .)
    x %<>% gsub(" RA$", "", .)
    x %<>% gsub(" A$", "", .)
    x %<>% gsub(" R$", "", .)
    x %<>% gsub(" MI$", "", .)
    x %<>% gsub(" C$", "", .)
    x %<>% gsub(" AN$", "", .)
    x %<>% gsub(" G$", "", .)
    x %<>% gsub(" B$", "", .)
    x %<>% gsub(" DE S$", "", .)
    x %<>% gsub(" DEL M$", "", .)
    x %<>% gsub(" DE M$", "", .)
    x %<>% gsub(" DE DO$", "", .)
    x %<>% gsub(" AR$", "", .)
    x %<>% gsub(" DEL PI$", "", .)
    x %<>% gsub(" DE CL$", "", .)
    x %<>% gsub(" DE CL ", " ", .)
    x %<>% gsub(" O ", " ", .)
    x %<>% gsub(" AR ", " ", .)
    x %<>% gsub(" DE DO ", " ", .)
    x %<>% gsub(" DEL M ", " ", .)
    x %<>% gsub(" DE M ", " ", .)
    x %<>% gsub(" DE S ", " ", .)
    x %<>% gsub(" B ", " ", .)
    x %<>% gsub(" G ", " ", .)
    x %<>% gsub(" AN ", " ", .)
    x %<>% gsub(" C ", " ", .)
    x %<>% gsub(" RA ", " ", .)
    x %<>% gsub(" A ", " ", .)
    x %<>% gsub(" D ", " ", .)
    x %<>% gsub(" R ", " ", .)
    x %<>% gsub(" MI ", " ", .)
    x %<>% gsub("^A ", "", .)
    x %<>% gsub("DELAO", "DE LA O", .)
    x %<>% gsub("\\s+$", "", .)
    x %<>% gsub("  ", " ", .)
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
  
##FIN##=========================================================================