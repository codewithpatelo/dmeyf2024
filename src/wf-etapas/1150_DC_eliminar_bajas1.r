cat( "ETAPA  z1150_DT_eliminar_bajas1.r  INIT\n")

#------------------------------------------------------------------------------
# PREPARACION INICIAL
#------------------------------------------------------------------------------

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full= TRUE, verbose= FALSE) # garbage collection


args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )


#------------------------------------------------------------------------------
# LIBRERIAS
#------------------------------------------------------------------------------
# Para leer parametros
require("yaml", quietly=TRUE)
require("data.table", quietly=TRUE)

#------------------------------------------------------------------------------
# Empieza Programa
#------------------------------------------------------------------------------
cat( "ETAPA  z1150_DT_eliminar_bajas1.r   START\n")
action_inicializar() 

#------------------------------------------------------------------------------
# Parametros
#------------------------------------------------------------------------------

# Definir los valores específicos para foto_mes que deseamos filtrar
valores_foto_mes <- envg$PARAM$EliminarBajas1$meses


#------------------------------------------------------------------------------
# Carga Dataset
#------------------------------------------------------------------------------
# Paso 1: Cargar el archivo desde la dirección especificada usando fread de data.table
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )
cat( "lectura del dataset\n")
cat(envg$PARAM$dataset, "\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

colnames(dataset)[which(!(sapply(dataset, typeof) %in% c("integer", "double")))]

GrabarOutput()

# Paso 2: Filtrar los datos para encontrar las filas que cumplan ambas condiciones:
#   - Los valores en foto_mes que correspondan a los meses especificados.
#   - El valor "BAJA+1" en la columna Clase_terciaria.
# Filtrar las filas que no cumplen ambas condiciones y eliminarlas
datos_filtrados <- dataset[!(foto_mes %in% valores_foto_mes & clase_ternaria == "BAJA+1")]


#------------------------------------------------------------------------------
# Guardo...
#------------------------------------------------------------------------------

# Paso 3: Guardar el resultado en un nuevo archivo comprimido en formato CSV
cat( "grabado del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(datos_filtrados,
       file = "dataset.csv.gz",
       logical01 = TRUE,
       sep = ","
)
cat( "Finalizado grabado del dataset\n" )


# copia la metadata sin modificar
cat( "grabado de metadata\n")
write_yaml( envg$PARAM$dataset_metadata, 
            file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
       file = "dataset.campos.txt",
       sep = "\t"
)

# Buscar filas que cumplan con las condiciones eliminadas
casos_restantes <- datos_filtrados[foto_mes %in% valores_foto_mes & clase_ternaria == "BAJA+1"]


# Paso 3: Comprobar si existen resultados en "casos_restantes"
if (nrow(casos_restantes) == 0) {
  cat("La verificación fue exitosa: No hay casos restantes con los valores de foto_mes y Clase_terciaria especificados.\n")
} else {
  cat("Advertencia: Existen", nrow(casos_restantes), "casos que cumplen las condiciones y no fueron eliminados.\n")
}

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 

cat( "ETAPA  z1150_DT_eliminar_bajas1.r  END\n")
