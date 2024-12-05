library(data.table)

# Directorio donde se encuentran los archivos
setwd("C:/Users/POO03/Documents/Patricio/datos_hibridaciones/entregas") # Cambia esta ruta a la carpeta de tus archivos

# Lista de archivos a procesar
archivos <- list.files(pattern = "*.csv") # Asume que los archivos son .csv

# Función para calcular la moda
calcular_moda <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Leer y combinar todos los archivos en una sola tabla
lista_datos <- lapply(archivos, fread)
datos_combinados <- rbindlist(lista_datos)

# Calcular la moda para cada numero_de_cliente
votaciones <- datos_combinados[, .(Predicted = calcular_moda(Predicted)), by = numero_de_cliente]

# Guardar el resultado en un archivo CSV
fwrite(votaciones, "super_votacion_kaggle_buenosv2.csv", row.names = FALSE)
