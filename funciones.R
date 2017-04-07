## Funciones para limpieza y procesamiento de la base ##

# Mediante esto cargamos las libraries requeridas para las funciones.

x <- c("stringr", "taRifx", "magrittr")
lapply(x, require, character.only = T)
remove(x)

# Esta función identifica las fracciones en un vector de caracteres (en la forma
# numero/numero y las transforma en un número)

fraccion.a.numero <- function(x) {
  if(is.character(x) == F) {
    stop("El argumento ocupa ser un vector de caracteres")
  }
  temp <- strsplit(x, split = "/")
  temp %<>% unlist()
  temp %<>% destring()
  return(temp[1] / temp[2])
}

# Esta función extrae todos los números, en forma numérica, que encuentra en un
# vector de caracteres y devuelve un vector numérico con los valores. Aquí falta
# hacer una función que transforme números escritos con letras en digitos y una
# forma de lograr discriminar que números se deben escoger basados en la palabra
# que le sigue, es decir, no tomar en cuenta el 3 en "durante 3 meses".

extraer.numeros <- function(x) {
  if(is.character(x) == F) {
    stop("El argumento ocupa ser un vector de caracteres")
  }
  num <- str_extract_all(x, "\\(?[0-9,/]+\\)?")[[1]]
  v <- length(grep("/", num))
  if(length(v) != 0) {
    for(i in 1:length(v)) {
      num[v[i]] %<>% fraccion.a.numero()
    }
  }
  return(destring(num, keep = "0-9./"))
}

# Esta función asigna un id numérico único por cada elemento único de un cierto
# vector.

asignar.id <- function(x) {
  id <- match(x, unique(x))
  return(id)
}

##FIN=========================================================================##