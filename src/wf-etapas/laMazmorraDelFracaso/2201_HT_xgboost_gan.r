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
  
  # para que una siguiente corrida pueda retomar
  if( file.exists("bayesiana.RDATA") ) {
    
    cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
        file = "z-Rcanresume.txt",
        append = TRUE
    )
  }
  
  param_completo <- c(envg$PARAM$xgb_basicos, x)
  param_completo$max_depth <- as.integer(param_completo$max_depth)
  param_completo$min_child_weight <- as.integer(param_completo$min_child_weight)
  
  param_completo$early_stopping_rounds <- as.integer(400 + 4 / param_completo$eta)
  
  
  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()
  set.seed(envg$PARAM$xgb_semilla, kind = "L'Ecuyer-CMRG")
  
  evals <- list(train = dtrain, eval = dvalidate)
  
  modelo_train <- xgb.train(
    params = param_completo,
    data = dtrain,
    nrounds = envg$PARAM$xgb_nrounds,
    evals = evals,
    feval = fganancia_xgb_meseta,
    maximize = TRUE,
    early_stopping_rounds = param_completo$early_stopping_rounds,
    verbose = 0
  )
  
  cat("\n")
  
  cant_corte <- vcant_optima[modelo_train$best_iter]
  
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
  
  cantidad_test_normalizada <- which.max(tbl[, gan_suavizada])
  
  rm(tbl)
  gc(verbose= FALSE)
  
  ganancia_test_normalizada <- ganancia_test
  
  # logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))
  
  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelo_train$best_iter
  xx$estimulos <- cantidad_test_normalizada
  xx$qsemillas <- 1L
  xx$ganancia <- ganancia_test_normalizada
  xx$metrica <- ganancia_test_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion
  
  
  superacion <- FALSE
  
  # voy grabando las mejores column importance
  if (ganancia_test_normalizada > GLOBAL_ganancia) {
    GLOBAL_ganancia <<- ganancia_test_normalizada
    tb_importancia <- as.data.table(xgb.importance(modelo_train))
    
    fwrite(tb_importancia,
           file = paste0("impo_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
           sep = "\t"
    )
    
    rm(tb_importancia)
    envg$OUTPUT$BO$mejor$iteracion <<- GLOBAL_iteracion
    envg$OUTPUT$BO$mejor$ganancia <<- GLOBAL_ganancia
    envg$OUTPUT$BO$mejor$metrica <<- GLOBAL_ganancia
    envg$OUTPUT$BO$mejor$arboles <<- modelo_train$best_iter
    GrabarOutput()
    mlog_log(xx, arch = "BO_log_mejor.txt")
    
    t <- format(Sys.time(), "%Y%m%d %H%M%S")
    cat( t, "\n",
         file = "z-Rcanbypass.txt",
         append = TRUE
    )
    
    superacion <- TRUE
  }
  
  mlog_log(xx, arch = "BO_log.txt", parentreplicate= superacion)
  
  cat( "Fin EstimarGanancia_xgboost()\n")
  set.seed(envg$PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  return(ganancia_test_normalizada)
}

#------------------------------------------------------------------------------
# esta es la funcion mas mistica de toda la asignatura
# sera explicada en  Laboratorio de Implementacion III

vcant_optima <- c()

fganancia_xgb_mesetaCV <- function(probs, dtrain) {
  # Obtener etiquetas y pesos desde el objeto dtrain
  vlabels <- getinfo(dtrain, "label")
  vpesos <- getinfo(dtrain, "weight")
  
  GLOBAL_arbol <<- GLOBAL_arbol + 1L
  
  # Crear tabla con probabilidades y ganancias
  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1, envg$PARAM$train$gan1, envg$PARAM$train$gan0)
  ))
  
  # Ordenar por probabilidad descendente
  setorder(tbl, -prob)
  tbl[, posicion := .I]
  
  # Calcular ganancias acumuladas y suavizadas
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = as.integer(envg$PARAM$train$meseta / envg$PARAM$xgb_crossvalidation_folds),
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]
  
  # Obtener máxima ganancia suavizada
  gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]
  pos <- which.max(tbl[, gan_suavizada])
  
  # Actualizar variables globales
  vcant_optima <<- c(vcant_optima, pos)
  
  if (GLOBAL_arbol %% (10 * envg$PARAM$xgb_crossvalidation_folds) == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan
    
    cat("\r")
    cat(
      "Cross Validate ", GLOBAL_iteracion, " ", " ",
      as.integer(GLOBAL_arbol / envg$PARAM$xgb_crossvalidation_folds), "  ",
      gan * envg$PARAM$xgb_crossvalidation_folds, "   ",
      GLOBAL_gan_max * envg$PARAM$xgb_crossvalidation_folds, "   "
    )
  }
  
  # Devolver la métrica personalizada
  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}

#------------------------------------------------------------------------------

EstimarGanancia_xgbCV <- function(x) {
  cat("Inicio EstimarGanancia_xgbCV()\n")
  gc(verbose = FALSE)
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  envg$OUTPUT$BO$iteracion_actual <<- GLOBAL_iteracion
  GrabarOutput()
  
  # Permitir retomar corridas previas
  if (file.exists("bayesiana.RDATA")) {
    cat(
      format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
      file = "z-Rcanresume.txt",
      append = TRUE
    )
  }
  
  param_completo <- c(envg$PARAM$xgb_basicos, x)
  param_completo$early_stopping_rounds <- as.integer(400 + 4 / param_completo$eta)
  
  vcant_optima <<- c()
  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  set.seed(envg$PARAM$xgb_semilla, kind = "L'Ecuyer-CMRG")
  
  modelocv <- xgb.cv(
    params = param_completo,
    data = dtrain,
    feval = fganancia_xgb_mesetaCV,
    stratified = TRUE,
    nfold = envg$PARAM$xgb_crossvalidation_folds,
    early_stopping_rounds = param_completo$early_stopping_rounds,
    verbose = FALSE
  )
  
  cat("\n")
  
  desde <- (modelocv$best_iteration - 1) * envg$PARAM$xgb_crossvalidation_folds + 1
  hasta <- desde + envg$PARAM$xgb_crossvalidation_folds - 1
  
  cant_corte <- as.integer(mean(vcant_optima[desde:hasta]) * envg$PARAM$xgb_crossvalidation_folds)
  
  ganancia <- modelocv$evaluation_log$test_ganancia_mean[modelocv$best_iteration]
  ganancia_normalizada <- ganancia * envg$PARAM$xgb_crossvalidation_folds
  
  if (ktest == TRUE) {
    # Crear modelo con los mejores parámetros
    param_completo$early_stopping_rounds <- NULL
    param_completo$nrounds <- modelocv$best_iteration
    
    modelo <- xgb.train(
      params = param_completo,
      data = dtrain,
      nrounds = param_completo$nrounds,
      verbose = FALSE
    )
    
    # Aplicar el modelo a los datos de testing
    prediccion <- predict(
      modelo,
      newdata = data.matrix(dataset_test[, campos_buenos, with = FALSE])
    )
    
    tbl <- copy(dataset_test[
      ,
      list("gan" = ifelse(
        get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos,
        envg$PARAM$train$gan1, envg$PARAM$train$gan0
      ))
    ])
    
    tbl[, prob := prediccion]
    setorder(tbl, -prob)
    
    tbl[, gan_acum := cumsum(gan)]
    tbl[, gan_suavizada := frollmean(
      x = gan_acum, n = envg$PARAM$train$meseta,
      align = "center", na.rm = TRUE, hasNA = TRUE
    )]
    
    # Actualizar ganancias y corte óptimo
    ganancia_normalizada <- tbl[, max(gan_suavizada, na.rm = TRUE)]
    cant_corte <- which.max(tbl[, gan_suavizada])
    
    rm(tbl)
    gc(verbose = FALSE)
  }
  
  # Guardar feature importance si se mejora la ganancia
  if (ganancia_normalizada > GLOBAL_ganancia) {
    GLOBAL_ganancia <<- ganancia_normalizada
    
    param_impo <- copy(param_completo)
    param_impo$early_stopping_rounds <- NULL
    param_impo$nrounds <- modelocv$best_iteration
    
    modelo <- xgb.train(
      params = param_impo,
      data = dtrain,
      nrounds = param_impo$nrounds,
      verbose = FALSE
    )
    
    tb_importancia <- as.data.table(xgb.importance(model = modelo))
    
    fwrite(tb_importancia,
           file = paste0("impo_", GLOBAL_iteracion, ".txt"),
           sep = "\t"
    )
    
    rm(tb_importancia)
    
    envg$OUTPUT$BO$mejor$iteracion <<- GLOBAL_iteracion
    envg$OUTPUT$BO$mejor$ganancia <<- GLOBAL_ganancia
    envg$OUTPUT$BO$mejor$metrica <<- GLOBAL_ganancia
    envg$OUTPUT$BO$mejor$arboles <<- modelocv$best_iteration
    GrabarOutput()
  }
  
  # Log final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))
  
  xx$early_stopping_rounds <- NULL
  xx$nrounds <- modelocv$best_iteration
  xx$estimulos <- cant_corte
  xx$qsemillas <- 1L
  xx$ganancia <- ganancia_normalizada
  xx$metrica <- ganancia_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion
  
  mlog_log(xx, arch = "BO_log.txt")
  set.seed(envg$PARAM$xgb_semilla, kind = "L'Ecuyer-CMRG")
  
  cat("Fin EstimarGanancia_xgbCV()\n")
  return(ganancia_normalizada)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
parametrizar  <- function( lparam )
{
  param_fijos  <- copy( lparam )
  hs  <- list()
  
  for( param  in  names( lparam ) )
  {
    if( length( lparam[[ param ]] ) > 1 )
    {
      desde  <- as.numeric( lparam[[ param ]][[1]]  )
      hasta  <- as.numeric( lparam[[ param ]][[2]]  )
      
      if( length( lparam[[ param ]] ) == 2 )
      {
        hs  <- append( hs,  
                       list( makeNumericParam( param, lower= desde, upper= hasta)  ) )
      } else {
        hs  <- append( hs, 
                       list( makeIntegerParam( param, lower= desde, upper= hasta) ) )
      }
      
      param_fijos[[ param ]] <- NULL  #lo quito 
    }
  }
  
  return( list( "param_fijos" =  param_fijos,
                "paramSet"    =  hs ) )
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Aquí empieza el programa
cat("ETAPA z2201_HT_xgboost_gan.r START\n")
action_inicializar()

# cargo las semillas
envg$PARAM$xgb_semilla <- envg$PARAM$semilla

# Apertura de los parámetros de XGBoost
apertura  <- parametrizar( envg$PARAM$xgb_param )
envg$PARAM$lgb_basicos <- apertura$param_fijos
envg$PARAM$xgb_basicos <- apertura$param_fijos
envg$PARAM$bo_xgb <- makeParamSet(params = apertura$paramSet)

envg$PARAM$xgb_basicos$seed <- envg$PARAM$semilla

# cargo el dataset donde voy a entrenar
envg$PARAM$dataset <- paste0("./", envg$PARAM$input, "/dataset_training.csv.gz")
envg$PARAM$dataset_metadata <- read_yaml(paste0("./", envg$PARAM$input, "/dataset_metadata.yml"))

cat("Lectura del dataset\n")
action_verificar_archivo(envg$PARAM$dataset)
cat("Iniciando lectura del dataset\n")
dataset <- fread(envg$PARAM$dataset)
cat("Finalizada lectura del dataset\n")

dataset[, azar := NULL]


# Verificaciones de campos
if (!("fold_train" %in% colnames(dataset))) {
  stop("Error, el dataset no tiene el campo fold_train \n")
}

if (!("fold_validate" %in% colnames(dataset))) {
  stop("Error, el dataset no tiene el campo fold_validate \n")
}

if (!("fold_test" %in% colnames(dataset))) {
  stop("Error, el dataset no tiene el campo fold_test \n")
}

if (dataset[fold_train == 1, .N] == 0) {
  stop("Error, en el dataset no hay registros con fold_train==1 \n")
}

GrabarOutput()

cat(envg$PARAM$exp_input, file = "TrainingStrategy.txt", append = FALSE)

# Defino la clase binaria clase01
cat("Creación clase01\n")
dataset[, clase01 := 0L]
dataset[get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase01_valor1, clase01 := 1L]

# Los campos que se pueden utilizar para la predicción
cat("Creación campos_buenos\n")
campos_buenos <- setdiff(
  copy(colnames(dataset)),
  c("clase01", envg$PARAM$dataset_metadata$clase, "fold_train", "fold_validate", "fold_test")
)

# La partición de train siempre va
cat("Creación dtrain\n")
dtrain <- xgb.DMatrix(
  data = data.matrix(dataset[fold_train == 1, campos_buenos, with = FALSE]),
  label = dataset[fold_train == 1, clase01],
  weight = dataset[
    fold_train == 1,
    ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, 1.0000001, 1.0)
  ]
)

envg$OUTPUT$train$ncol <- ncol(dtrain)
envg$OUTPUT$train$nrow <- nrow(dtrain)
envg$OUTPUT$train$periodos <- dataset[fold_train == 1, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

kvalidate <- FALSE
ktest <- FALSE
kcrossvalidation <- TRUE

# Si hay que hacer validación
cat("Creación dvalidate\n")
if (dataset[fold_train == 0 & fold_test == 0 & fold_validate == 1, .N] > 0) {
  kcrossvalidation <- FALSE
  kvalidate <- TRUE
  dvalidate <- xgb.DMatrix(
    data = data.matrix(dataset[fold_validate == 1, campos_buenos, with = FALSE]),
    label = dataset[fold_validate == 1, clase01],
    weight = dataset[
      fold_validate == 1,
      ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, 1.0000001, 1.0)
    ]
  )
  
  envg$OUTPUT$validate$ncol <- ncol(dvalidate)
  envg$OUTPUT$validate$nrow <- nrow(dvalidate)
  
  envg$OUTPUT$validate$periodos <- dataset[
    fold_validate == 1,
    length(unique(get(envg$PARAM$dataset_metadata$periodo)))
  ]
}

# Si hay que hacer testing
if (dataset[fold_train == 0 & fold_validate == 0 & fold_test == 1, .N] > 0) {
  cat( "creacion testing\n")
  ktest <- TRUE
  campos_buenos_test <- setdiff(
    copy(colnames(dataset)),
    c("fold_train", "fold_validate", "fold_test")
  )
  
  dataset_test <- dataset[fold_test == 1, campos_buenos_test, with = FALSE]
  
  envg$OUTPUT$test$ncol <- ncol(dataset_test)
  envg$OUTPUT$test$nrow <- nrow(dataset_test)
  envg$OUTPUT$test$periodos <- dataset_test[, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]
  
}

# Inicializo MLFlow
mlog_init()

if (kcrossvalidation) {
  datahash <- mlog_table_hash(dataset[, envg$PARAM$dataset_metadata$primarykey, with = FALSE])
  mlflow_exp_det <- paste0("/xval-", datahash)
} else {
  datahash <- mlog_table_hash(dataset[fold_test == 1, envg$PARAM$dataset_metadata$primarykey, with = FALSE])
  mlflow_exp_det <- paste0("/test-", datahash)
}

mlog_addfile("BO_log.txt",
             mlflow_exp = mlflow_exp_det,
             mlflow_run = envg$PARAM$experimento,
             cols_fijas = list(expw = envg$PARAM$experimento_largo))

rm(dataset)
gc(verbose = FALSE)

# si ya existe el archivo log, traigo hasta donde procese
if (file.exists("BO_log.txt")) {
  tabla_log <- fread("BO_log.txt")
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_ganancia <- tabla_log[, max(ganancia)]
  rm(tabla_log)
} else {
  GLOBAL_iteracion <- 0L
  GLOBAL_ganancia <- -Inf
}


# Aqui comienza la configuracion de mlrMBO


envg$OUTPUT$crossvalidation <- kcrossvalidation
GrabarOutput()

# deobo hacer cross validation o  Train/Validate/Test
if (kcrossvalidation) {
  funcion_optimizar <- EstimarGanancia_xgboostCV
} else {
  funcion_optimizar <- EstimarGanancia_xgboost
}


configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = envg$PARAM$bo_xgb, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# archivo donde se graba y cada cuantos segundos
ctrl <- makeMBOControl(
  save.on.disk.at.time = 60,
  save.file.path = "bayesiana.RDATA"
)

ctrl <- setMBOControlTermination(ctrl,
                                 iters = envg$PARAM$bo_iteraciones
) # cantidad de iteraciones

ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# establezco la funcion que busca el maximo
surr.km <- makeLearner("regr.km",
                       predict.type = "se",
                       covtype = "matern3_2",
                       control = list(trace = TRUE)
)

surr.km <- makeLearner("regr.km",
                       predict.type = "se",
                       covtype = "matern3_2",
                       optim.method = "BFGS",
                       nugget.estim = TRUE,
                       jitter = TRUE,
                       control = list(trace = TRUE)
)


# Aqui inicio la optimizacion bayesiana
set.seed(envg$PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
if (!file.exists("bayesiana.RDATA")) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  # si ya existe el archivo RDATA,
  # debo continuar desde el punto hasta donde llegue
  #  usado para cuando se corta la virtual machine
  run <- mboContinue("bayesiana.RDATA") # retomo en caso que ya exista
}

#------------------------------------------------------------------------------
BO_log <- fread("BO_log.txt")
envg$OUTPUT$ganancia_max <- BO_log[, max(ganancia, na.rm = TRUE)]

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()
#------------------------------------------------------------------------------

# ya no tiene sentido retomar, se termino el trabajo
file.remove("z-Rcanresume.txt")
#------------------------------------------------------------------------------

# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("BO_log.txt")) 