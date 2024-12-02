#!/usr/bin/env Rscript
cat("ETAPA  z2301_FM_final_models_xgboost.r  INIT\n")

# Workflow  final_models con XGBoost

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose = FALSE) # garbage collection

require("data.table", quietly = TRUE)
require("yaml", quietly = TRUE)
require("primes", quietly = TRUE)
require("xgboost", quietly = TRUE)

# cargo la librería
args <- commandArgs(trailingOnly = TRUE)
source(paste0(args[1], "/src/lib/action_lib.r"))

#------------------------------------------------------------------------------
# grabo la importancia de variables
grabar_importancia <- function(modelo_final, modelo_rank, iteracion_bayesiana) {
  tb_importancia <- xgb.importance(model = modelo_final)
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
# Aquí empieza el programa
cat("ETAPA  z2301_FM_final_models_xgboost.r  START\n")
action_inicializar()

# generar semillas
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(envg$PARAM$semilla)
envg$PARAM$semillas <- sample(primos)[1:envg$PARAM$qsemillas]

GrabarOutput()

# leer la salida de la optimización bayesiana
arch_log <- paste0("./", envg$PARAM$input[1], "/BO_log.txt")
action_verificar_archivo(arch_log)
tb_log <- fread(arch_log)
setorderv(tb_log, "metrica", envg$PARAM$metrica_order)

# leer el dataset para entrenar el modelo final
arch_dataset <- paste0("./", envg$PARAM$input[2], "/dataset_train_final.csv.gz")
cat("lectura dataset_train_final.csv.gz\n")
action_verificar_archivo(arch_dataset)
dataset <- fread(arch_dataset)
envg$PARAM$dataset_metadata <- read_yaml(paste0("./", envg$PARAM$input[2], "/dataset_metadata.yml"))

campos_buenos <- setdiff(colnames(dataset), c(envg$PARAM$dataset_metadata$clase, "clase01"))

dataset[, clase01 := 
  ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase01_valor1, 1, 0)]

# inicializar tabla de modelos si no existe
if (file.exists("tb_modelos.txt")) {
  tb_modelos <- fread("tb_modelos.txt")
} else {
  tb_modelos <- data.table(
    rank = integer(),
    iteracion_bayesiana = integer(),
    semilla = integer(),
    isem = integer(),
    archivo = character()
  )
}

cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
    file = "z-Rcanresume.txt",
    append = TRUE)

imodelo <- 0L
for (modelo_rank in envg$PARAM$modelos_rank) {
  imodelo <- imodelo + 1L
  cat("\nmodelo_rank: ", modelo_rank, ", semillas: ")
  envg$OUTPUT$status$modelo_rank <- modelo_rank

  parametros <- as.list(copy(tb_log[modelo_rank]))
  iteracion_bayesiana <- parametros$iteracion_bayesiana

  # preparar datos de XGBoost
  cat("creo xgb.DMatrix\n")
  dtrain <- xgb.DMatrix(
    data = data.matrix(dataset[, campos_buenos, with = FALSE]),
    label = dataset[, clase01]
  )

  ganancia <- parametros$ganancia

  # eliminar parámetros que no son de XGBoost
  parametros$experimento <- NULL
  parametros$cols <- NULL
  parametros$rows <- NULL
  parametros$fecha <- NULL
  parametros$estimulos <- NULL
  parametros$ganancia <- NULL
  parametros$metrica <- NULL
  parametros$iteracion_bayesiana <- NULL

  # parámetros adicionales para XGBoost
  parametros$objective <- "binary:logistic"
  parametros$eval_metric <- "logloss"

  sem <- 0L

  for (vsemilla in envg$PARAM$semillas) {
    sem <- sem + 1L
    cat(sem, " ")
    envg$OUTPUT$status$sem <- sem
    GrabarOutput()

    # configurar semilla
    parametros$seed <- vsemilla

    nombre_raiz <- paste0(
      sprintf("%02d", modelo_rank),
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

    # entrenar modelo si no existe
    if (!file.exists(arch_modelo)) {
      cat("\nentrenando modelo = ", sem, "  .")
      set.seed(parametros$seed, kind = "L'Ecuyer-CMRG")
      param_preparado <- list(
        booster = parametros$booster,
        objective = parametros$objective,
        tree_method = parametros$tree_method,
        max_depth = parametros$max_depth
        gamma = parametros$gamma,
        alpha = parametros$alpha,
        lambda = parametros$lamda,
        min_child_weight = parametros$min_child_weight,
        tweedie_variance_power = parametros$tweedie_variance_power,
        max_delta_step = parametros$max_delta_step,
        max_bin = parametros$max_bin,
        subsample = parametros$subsample,
        scale_pos_weight = parametros$scale_pos_weight,
        eta = parametros$eta,
        colsample_bytree= parametros$colsample_bytree,
     )
      modelo_final <- xgb.train(
        params = param_preparado,
        data = dtrain,
        nrounds = parametros$nrounds,
        verbose = parametros$verbose
      )
      cat(" ...Fin.")

      # guardar modelo
      xgb.save(modelo_final, arch_modelo)

      # guardar importancia de variables
      if (sem == 1) {
        cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
            file = "z-Rcanresume.txt",
            append = TRUE)

        grabar_importancia(modelo_final, modelo_rank, iteracion_bayesiana)
      }

      # agregar a tabla de modelos
      tb_modelos <- rbind(tb_modelos,
        list(modelo_rank,
          iteracion_bayesiana,
          vsemilla,
          sem,
          arch_modelo
        ))

      fwrite(tb_modelos,
        file = "tb_modelos.txt",
        sep = "\t"
      )
    }
  }
}

#------------------------------------------------------------------------------
# guardar metadata
cat("grabar metadata\n")

write_yaml(envg$PARAM$dataset_metadata,
  file = "dataset_metadata.yml")

#------------------------------------------------------------------------------
# finalizar
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

action_finalizar(archivos = c())
cat("ETAPA  z2301_FM_final_models_xgboost.r  END\n")
