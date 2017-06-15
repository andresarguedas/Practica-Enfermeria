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
  
##FIN##=========================================================================