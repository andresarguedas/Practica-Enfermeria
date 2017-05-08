##Funciones para limpieza y procesamiento de la base##==========================
  
  # Mediante esto cargamos las libraries requeridas para las funciones.
  
  x <- c("stringr", "taRifx", "magrittr")
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
                    veintinueve = 29, treinta = 30)
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
        ##AGREGAR SOLUCIÓN PARA PUNTOS ENTRE NÚMEROS##
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
  
  # Esta función asigna un ID numérico único por cada elemento único de un 
  # cierto vector. Aquí el argumento "x" sería la cedula y el argumento "y" el 
  # nombre, para poder solucionar los casos de NA's. Cuando na.especial = T 
  # entonces se le asigna un ID especifico a cada uno de los NA's, 
  # na.especial = F entonces el argumento "y" no es necesario. La función 
  # regresa un vector con los IDs.
  
  asignar.ID <- function(x, y, na.especial = T) {
    ID <- match(x, unique(x))
    if(na.especial) {
      for(i in 1:length(unique(y[is.na(x)]))) {
        IDt <- max(ID) + 1
        if(length(unique(x[y == unique(y[is.na(x)])[i]])) == 1) {
          ID[y == unique(y[is.na(x)])[i]] <- IDt
        } else {
          v <- (x[y == unique(y[is.na(x)])[i]])[!is.na(x[y == unique(y[is.na(x)])[i]])]
          for(j in 1:length(v)) {
            ID[x == v[j]] <- IDt
          }
        }
      }
    }
    return(ID)
  }

##FIN##=========================================================================