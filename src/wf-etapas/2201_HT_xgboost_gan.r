#!/usr/bin/env Rscript
cat("ETAPA  z2201_HT_xgboost_gan.r  INIT\n")

# Hyperparameter Tuning  xgboost

# inputs
#  * dataset con partición train, validate, test
#  * hiperparámetros fijos y variables (que van a la Bayesian Optimization)
# output  
#   archivo  BO_log.txt  resultado de la Bayesian Optimization

# limpio la memoria
rm(list = ls(all.names = TRUE)) 
gc(full = TRUE, verbose=FALSE)

require("data.table", quietly=TRUE)
require("rlist", quietly=TRUE)
require("yaml", quietly=TRUE)

require("xgboost", quietly=TRUE)

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging", quietly=TRUE)
require("mlrMBO", quietly=TRUE)

# cargo la librería
args <- commandArgs(trailingOnly=TRUE)
source(paste0(args[1], "/src/lib/mlog.r"))
source(paste0(args[1], "/src/lib/action_lib.r"))

#------------------------------------------------------------------------------#

GLOBAL_arbol <- 0L
GLOBAL_gan_max <- -Inf
vcant_optima <- c()

fganancia_xgb_meseta <- function(preds, dtrain) {
  vlabels <- getinfo(dtrain, "label")
  vweights <- getinfo(dtrain, "weight")

  GLOBAL_arbol <<- GLOBAL_arbol + 1
  tbl <- as.data.table(list(
    "prob" = preds,
    "gan" = ifelse(vlabels == 1 & vweights > 1, envg$PARAM$train$gan1, envg$PARAM$train$gan0)
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, 
    n = envg$PARAM$train$meseta, 
    align = "center", 
    na.rm = TRUE, 
    hasNA = TRUE
  )]

  gan <- max(tbl$gan_suavizada, na.rm = TRUE)
  pos <- which.max(tbl$gan_suavizada)
  vcant_optima <<- c(vcant_optima, pos)

  if (GLOBAL_arbol %% 10 == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan
    cat("\r")
    cat(
      "Validate ", GLOBAL_iteracion, " ", 
      GLOBAL_arbol, "  ", gan, "   ", GLOBAL_gan_max, "   "
    )
  }

  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}

#------------------------------------------------------------------------------#

EstimarGanancia_xgboost <- function(x) {
  cat("Inicio EstimarGanancia_xgboost()\n")
  gc(verbose=FALSE)
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  envg$OUTPUT$BO$iteracion_actual <<- GLOBAL_iteracion
  GrabarOutput()

  param_completo <- c(envg$PARAM$xgb_basicos, x)
  param_completo$max_depth <- as.integer(param_completo$max_depth)
  param_completo$min_child_weight <- as.integer(param_completo$min_child_weight)
  param_completo$nthread <- envg$PARAM$xgb_nthread

  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()
  set.seed(envg$PARAM$xgb_semilla, kind = "L'Ecuyer-CMRG")

  dtrain <- xgb.DMatrix(
    data = data.matrix(dataset_train[, campos_buenos, with = FALSE]), 
    label = dataset_train[[envg$PARAM$dataset_metadata$clase]]
  )

  dvalidate <- xgb.DMatrix(
    data = data.matrix(dataset_validate[, campos_buenos, with = FALSE]), 
    label = dataset_validate[[envg$PARAM$dataset_metadata$clase]]
  )

  watchlist <- list(train = dtrain, eval = dvalidate)

  modelo_train <- xgb.train(
    params = param_completo,
    data = dtrain,
    nrounds = envg$PARAM$xgb_nrounds,
    watchlist = watchlist,
    feval = fganancia_xgb_meseta,
    maximize = TRUE,
    early_stopping_rounds = param_completo$early_stopping_rounds,
    verbose = 0
  )

  cat("\n")

  # Calcular la ganancia final
  prediccion <- predict(
    modelo_train, 
    newdata = data.matrix(dataset_test[, campos_buenos, with = FALSE])
  )

  tbl <- copy(dataset_test[, list("gan" = 
    ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, 
           envg$PARAM$train$gan1, 
           envg$PARAM$train$gan0))])

  tbl[, prob := prediccion]
  setorder(tbl, -prob)
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, 
    n = envg$PARAM$train$meseta, 
    align = "center", 
    na.rm = TRUE, 
    hasNA = TRUE
  )]

  ganancia_test <- max(tbl$gan_suavizada, na.rm = TRUE)

  # Logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))
  xx$ganancia <- ganancia_test
  xx$metrica <- ganancia_test
  xx$iteracion_bayesiana <- GLOBAL_iteracion

  if (ganancia_test > GLOBAL_ganancia) {
    GLOBAL_ganancia <<- ganancia_test
    envg$OUTPUT$BO$mejor$iteracion <<- GLOBAL_iteracion
    envg$OUTPUT$BO$mejor$ganancia <<- GLOBAL_ganancia
    GrabarOutput()
  }

  mlog_log(xx, arch = "BO_log.txt")
  cat("Fin EstimarGanancia_xgboost()\n")
  return(ganancia_test)
}
