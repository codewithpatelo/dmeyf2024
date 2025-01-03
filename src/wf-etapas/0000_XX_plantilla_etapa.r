cat( "ETAPA  z000_XX_<etapa>.r  INIT\n")

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

#------------------------------------------------------------------------------
# FUNCIONES 
#------------------------------------------------------------------------------
Funcion_Principal_Etapa <- function (atr1, atr2, atr3) {

}

funcion_utilidad <- function (atr1, atr2, atr3) {
    
}

#------------------------------------------------------------------------------
# Empieza Programa
#------------------------------------------------------------------------------
cat( "ETAPA  z000_XX_<etapa>.r   START\n")
action_inicializar() 

#------------------------------------------------------------------------------
# Parametros
#------------------------------------------------------------------------------

# Parametros pasados del wf...
param1 <- envg$PARAM$NombreEtapa$param1
param2 <- envg$PARAM$NombreEtapa$param2


#------------------------------------------------------------------------------
# Carga Dataset
#------------------------------------------------------------------------------
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )
cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

colnames(dataset)[which(!(sapply(dataset, typeof) %in% c("integer", "double")))]

GrabarOutput()


# <Hago lo que necesito con el dataset>
# Funcion_Principal_Etapa()


#------------------------------------------------------------------------------
# Guardo...
#------------------------------------------------------------------------------

# grabo el dataset
cat( "grabado del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
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

cat( "ETAPA  z000_XX_<etapa>.r  END\n")
