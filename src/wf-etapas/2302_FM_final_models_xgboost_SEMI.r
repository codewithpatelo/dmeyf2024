#!/usr/bin/env Rscript
cat( "ETAPA  z2302_FM_final_models_xgboost_semillerio.r  INIT\n")

# Workflow  final_models

# inputs
#  * dataset  final_training
#  * archivo BO_log.txt de una Bayesian Optimization
#  * ranks para los que se tiene interes en generar modelos
#  * cantidad de semillas
# output  
#   para cada rank, semilla  un   modelos  .model


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose=FALSE) # garbage collection

require("data.table")
require("yaml")
require("primes")

require("xgboost")

#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
# grabo la importancia de variables

grabar_importancia <- function(modelo_final, modelo_rank, iteracion_bayesiana) {
  tb_importancia <- as.data.table(xgb.importance(modelo_final))
  fwrite(tb_importancia,
         file = paste0(
           "impo_",
           sprintf("%02d", modelo_rank),
           "_",
           sprintf("%03d", iteracion_bayesiana),
           ".txt"
         ),
         sep = "\t"
  )
  
  rm(tb_importancia)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "ETAPA  z2302_FM_final_models_xgboost_semillerio.r  START\n")
action_inicializar() 

# genero las semillas con las que voy a trabajar
#  ninguna de ellas es exactamente la original del alumno
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(envg$PARAM$semilla, kind = "L'Ecuyer-CMRG")
total_semillas <- envg$PARAM$semillerio * envg$PARAM$repeticiones_exp
# me quedo con PARAM$semillerio  primos al azar
envg$PARAM$semillas <- sample(primos)[1:total_semillas]


GrabarOutput()

# leo la salida de la optimizacion bayesiana
# En PARAM$input[1]  tango el nombre del experimento de Hyperparameter Tuning
arch_log <- paste0( "./", envg$PARAM$input[1], "/BO_log.txt")
action_verificar_archivo( arch_log )
tb_log <- fread(arch_log)
setorderv(tb_log, "metrica", envg$PARAM$metrica_order)


# leo el dataset donde voy a entrenar el modelo final
# En PARAM$input[2]  tango el nombre del experimento de TS Training Strategy
arch_dataset <- paste0("./", envg$PARAM$input[2], "/dataset_train_final.csv.gz")
cat( "lectura dataset_train_final.csv.gz\n")
action_verificar_archivo( arch_dataset )
dataset <- fread(arch_dataset)
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input[2], "/dataset_metadata.yml" ) )


campos_buenos <- setdiff(colnames(dataset), c(envg$PARAM$dataset_metadata$clase, "clase01"))

dataset[ , clase01 := 
           ifelse( get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase01_valor1, 1, 0 ) ]

# genero un modelo para cada uno de las modelos_qty MEJORES
# iteraciones de la Bayesian Optimization
vganancias_suavizadas <- c()


if( file.exists( "tb_modelos.txt" ) ){
  tb_modelos <- fread( "tb_modelos.txt" )
} else {
  tb_modelos <- data.table( 
    rank= integer(),
    iteracion_bayesiana= integer(),
    repeticion= integer(),
    semilla= integer(),
    isem= integer(),
    archivo= character() )
}

# puedo resumir
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
    file = "z-Rcanresume.txt",
    append = TRUE
)

imodelo <- 0L
for (modelo_rank in envg$PARAM$modelos_rank) {
  imodelo <- imodelo + 1L
  cat("\nmodelo_rank: ", modelo_rank, ", semillas: ")
  envg$OUTPUT$status$modelo_rank <- modelo_rank
  
  parametros <- as.list(copy(tb_log[modelo_rank]))
  iteracion_bayesiana <- parametros$iteracion_bayesiana
  
  
  # creo CADA VEZ el dataset de xgbm
  cat( "creo xgb.Matrix\n")
  dtrain <- xgb.DMatrix(
    data = data.matrix(dataset[, campos_buenos, with = FALSE]),
    label = dataset[, clase01],
  )
  
  
  ganancia <- parametros$ganancia
  
  # elimino los parametros que no son de xgbm
  parametros$experimento <- NULL
  parametros$cols <- NULL
  parametros$rows <- NULL
  parametros$fecha <- NULL
  parametros$estimulos <- NULL
  parametros$ganancia <- NULL
  parametros$metrica <- NULL
  parametros$iteracion_bayesiana <- NULL
  
  
  if( "eta_log" %in% names(parametros) )
    parametros$eta <- 2.0 ^ parametros$eta_log
  
  if( "colsample_bytree_log" %in% names(parametros) )
    parametros$colsample_bytree <- 2.0 ^ parametros$colsample_bytree_log
  
  if( "nrounds_log" %in% names(parametros) )
    parametros$nrounds <- pmax( 1L, as.integer( round( 2.0 ^ parametros$nrounds_log ) ))
  
  
  if( "min_child_weight_log" %in% names(parametros) )
    parametros$min_child_weight <- pmax( 1L, as.integer( round(2.0 ^ parametros$min_child_weight_log)))
  
  
  
  
  #  parametros$nrounds  <- 10  # esta linea es solo para pruebas
  
  for( irepeticion in 1:envg$PARAM$repeticiones_exp )
  {
    desde <- envg$PARAM$semillerio * (irepeticion -1 ) + 1
    hasta <- desde + envg$PARAM$semillerio -1
    
    sem <- 0L
    
    for (vsemilla in envg$PARAM$semillas[desde:hasta] )
    {
      sem <- sem + 1L
      cat(sem, " ")
      envg$OUTPUT$status$sem <- sem
      GrabarOutput()
      
      # Utilizo la semilla definida en este script
      parametros$seed <- vsemilla
      
      nombre_raiz <- paste0(
        sprintf("%02d", modelo_rank),
        "_",
        sprintf("%03d", irepeticion),
        "_",
        sprintf("%03d", iteracion_bayesiana),
        "_s",
        parametros$seed
      )
      
      arch_modelo <- paste0(
        "modelo_",
        nombre_raiz,
        ".model"
      )
      
      # genero el modelo entrenando en los datos finales
      #  en caso que ya no exista
      if( !file.exists( arch_modelo ) )
      {
        cat( "\nentrenando modelo = ", sem, "  ." )
        set.seed(parametros$seed, kind = "L'Ecuyer-CMRG")
        print(parametros)
        param_preparado <- list(
          booster = parametros$booster,
          objective = parametros$objective,
          tree_method = parametros$tree_method,
          max_depth = parametros$max_depth,
          eval_metric = "auc",
          gamma = parametros$gamma,
          alpha = parametros$alpha,
          reg_lambda = parametros$lambda,
          min_child_weight = parametros$min_child_weight,
          max_delta_step = parametros$max_delta_step,
          max_bin = parametros$max_bin,
          subsample = parametros$subsample,
          scale_pos_weight = parametros$scale_pos_weight,
          eta = parametros$eta,
          colsample_bytree = parametros$colsample_bytree
        )
        
        modelo_final <- xgb.train(
          data = dtrain,
          param = param_preparado,
          nrounds = parametros$nrounds,
          verbose = parametros$verbose
        )
        
        cat( " ...Fin." )
        
        # grabo el modelo, achivo .model
        xgb.save(modelo_final,
                 fname = arch_modelo
        )
        
        # creo y grabo la importancia de variables, solo para la primer semilla
        if (sem == 1) {
          cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
              file = "z-Rcanresume.txt",
              append = TRUE
          )
          
          grabar_importancia(modelo_final, modelo_rank, iteracion_bayesiana)
        }
        
        # Agrego a tb_semillas
        tb_modelos <- rbind( tb_modelos,
                             list(modelo_rank,
                                  iteracion_bayesiana,
                                  irepeticion,
                                  vsemilla,
                                  sem,
                                  arch_modelo
                             ))
        
        fwrite( tb_modelos,
                file = "tb_modelos.txt",
                sep ="\t"
        )
        
        rm( "modelo_final" )
        gc(verbose= FALSE)
      }
    }
  }
  
  rm( "dtrain" )
  gc(verbose= FALSE)
}

#------------------------------------------------------------------------------
# copia la metadata sin modificar
cat( "grabar metadata\n")

write_yaml( envg$PARAM$dataset_metadata, 
            file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c()) 
cat( "ETAPA  z2302_FM_final_models_xgboost_semillerio.r  END\n")
