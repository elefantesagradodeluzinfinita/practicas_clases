install.packages("stringr")
library("stringr")
install.packages("dplyr")
install.packages("lubridate")

library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)

prueba <- read_table("D:/epa-http.csv", col_names = FALSE)

colnames(prueba) <- c("b", "c", "d", "e", "f", "g", "h")

#Cuales son las dimensiones del dataset cargado (número de filas)
nrow(prueba)

#Cuales son las dimensiones del dataset cargado (número de columnas)
ncol(prueba)

#De las diferentes IPs de origen accediendo al servidor, ¿cuantas pertenecen a una IP claramente educativa (que contenga ".edu")?
x <- filter(prueba, str_detect(prueba$b, "\\.edu"))
nrow(x)

#De todas las peticiones recibidas por el servidor cual es la hora en la que hay mayor volumen de peticiones HTTP de tipo "GET"?
zz <- filter(prueba , str_detect(prueba$d,"GET"))
date <- as.POSIXct(zz$c, format = "[%d:%H:%M:%OS]")
datehora <- format(date, format = "%H")
conteo <- table(datehora)
cadena_mas_comun <- names(conteo)[which.max(conteo)]
cat("La cadena de texto con más repeticiones en la columna de texto es:", cadena_mas_comun)

#De las peticiones hechas por instituciones educativas (.edu), ¿Cuantos bytes en total se han transmitido, en peticiones de descarga de ficheros de texto ".txt"?
zz <- filter(prueba , str_detect(prueba$b,"\\.edu"))
xx <- filter(zz , str_detect(zz$e,"txt"))
sumat <- sum(as.numeric(xx$h), na.rm=TRUE)
cat("La suma de bytes de los archivos txt de .edu es:", sumat)

#Si separamos la petición en 3 partes (Tipo, URL, Protocolo), usando str_split y el separador " " (espacio), ¿cuantas peticiones buscan directamente la URL = "/"?
MATRIZ_URL <- prueba[grepl("^/$", prueba$e), ]
nrow(MATRIZ_URL)

#¿Cuantas peticiones NO tienen como protocolo "HTTP/0.2"?
MATRIZ_UR1L <- prueba[!grepl("HTTP/0.2", prueba$f), ]
nrow(MATRIZ_UR1L)