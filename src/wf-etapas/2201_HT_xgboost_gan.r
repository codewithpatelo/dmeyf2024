#!/usr/bin/env Rscript
cat("ETAPA z2201_HT_xgboost_gan.r INIT\n")

# Hyperparameter Tuning para xgboost

# Limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose = FALSE) # garbage collection

require("data.table", quietly = TRUE)
require("rlist", quietly = TRUE)
require("yaml", quietly = TRUE)

require("xgboost", quietly = TRUE)

# Paquetes necesarios para la Bayesian Optimization
require("DiceKriging", quietly = TRUE)
require("mlrMBO", quietly = TRUE)

# Cargo la librería personalizada
args <- commandArgs(trailingOnly = TRUE)
source(paste0(args[1], "/src/lib/mlog.r"))
source(paste0(args[1], "/src/lib/action_lib.r"))

#------------------------------------------------------------------------------

GLOBAL_arbol <- 0L
GLOBAL_gan_max <- -Inf
vcant_optima <- c()

# Definir la función de ganancia para xgboost
fganancia_xgb_meseta <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  weights <- getinfo(dtrain, "weight")

  GLOBAL_arbol <<- GLOBAL_arbol + 1

  tbl <- as.data.table(list(
    "pred" = preds,
    "gan" = ifelse(labels == 1 & weights > 1, envg$PARAM$train$gan1, envg$PARAM$train$gan0)
  ))

  setorder(tbl, -pred)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = envg$PARAM$train$meseta,
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]

  gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]

  pos <- which.max(tbl[, gan_suavizada])
  vcant_optima <<- c(vcant_optima, pos)

  if (GLOBAL_arbol %% 10 == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan

    cat("\r")
    cat(
      "Validate ", GLOBAL_iteracion, " ", " ",
      GLOBAL_arbol, "  ", gan, "   ", GLOBAL_gan_max, "   "
    )
  }

  return(list(
    "metric" = "ganancia",
    "value" = gan
  ))
}

#------------------------------------------------------------------------------

EstimarGanancia_xgboost <- function(x) {
  cat("Inicio EstimarGanancia_xgboost()\n")
  gc(verbose = FALSE)
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  envg$OUTPUT$BO$iteracion_actual <<- GLOBAL_iteracion
  GrabarOutput()

  # Combino los parámetros básicos y los variables que vienen en x
  param_completo <- c(envg$PARAM$xgb_basicos, x)

  dtrain <- xgb.DMatrix(
    data = data.matrix(dataset_train[, campos_buenos, with = FALSE]),
    label = dataset_train[[envg$PARAM$dataset_metadata$clase]],
    weight = dataset_train[[envg$PARAM$train$peso]]
  )

  dvalid <- xgb.DMatrix(
    data = data.matrix(dataset_validate[, campos_buenos, with = FALSE]),
    label = dataset_validate[[envg$PARAM$dataset_metadata$clase]],
    weight = dataset_validate[[envg$PARAM$train$peso]]
  )

  watchlist <- list(train = dtrain, valid = dvalid)

  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()

  set.seed(envg$PARAM$xgb_semilla)
  modelo <- xgb.train(
    params = param_completo,
    data = dtrain,
    nrounds = 10000,
    watchlist = watchlist,
    feval = fganancia_xgb_meseta,
    maximize = TRUE,
    early_stopping_rounds = 400,
    verbose = 0
  )

  # Evaluar en el conjunto de prueba
  dtest <- xgb.DMatrix(
    data = data.matrix(dataset_test[, campos_buenos, with = FALSE])
  )

  preds <- predict(modelo, dtest)

  tbl <- copy(dataset_test[, list("gan" =
    ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos,
      envg$PARAM$train$gan1,
      envg$PARAM$train$gan0
    ))])

  tbl[, pred := preds]
  setorder(tbl, -pred)
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = envg$PARAM$train$meseta,
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]

  ganancia_test <- tbl[, max(gan_suavizada, na.rm = TRUE)]
  ganancia_test_normalizada <- ganancia_test

  # Loguear los resultados
  ds <- list("cols" = ncol(dataset_train), "
