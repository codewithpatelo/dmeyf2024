# limpio la memoria
format(Sys.time(), "%a %b %d %X %Y")
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

dir.create("~/buckets/b1/exp/lineademuertex/", showWarnings = FALSE)
setwd( "~/buckets/b1/exp/lineademuertex/" )

require( "data.table" )

# leo el dataset
dataset <- fread("~/buckets/b1/datasets/competencia_03_crudo.csv.gz" )

# calculo el periodo0 consecutivo
setorder( dataset, numero_de_cliente, foto_mes )
dataset[, periodo0 := as.integer(foto_mes/100)*12 +  foto_mes%%100]

# calculo topes
periodo_ultimo <- dataset[, max(periodo0) ]
periodo_anteultimo <- periodo_ultimo - 1

# calculo los leads de orden 1 y 2
dataset[, c("periodo1", "periodo2") :=
          shift(periodo0, n=1:2, fill=NA, type="lead"),  numero_de_cliente ]

# assign most common class values = "CONTINUA"
dataset[ periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA" ]

# calculo BAJA+1
dataset[ periodo0 < periodo_ultimo &
           ( is.na(periodo1) | periodo0 + 1 < periodo1 ),
         clase_ternaria := "BAJA+1" ]

# calculo BAJA+2
dataset[ periodo0 < periodo_anteultimo & (periodo0+1 == periodo1 )
         & ( is.na(periodo2) | periodo0 + 2 < periodo2 ),
         clase_ternaria := "BAJA+2" ]

dataset[, c("periodo0", "periodo1", "periodo2") := NULL ]

tbl <- dataset[, .N, list(foto_mes, clase_ternaria)]
setorder(tbl, foto_mes, clase_ternaria)
tbl


# Feature Engineering Historico
cols_lagueables <- copy( setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
) )


dataset[, 
        paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
        by = numero_de_cliente,
        .SDcols = cols_lagueables
]

dataset[, 
        paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
        by = numero_de_cliente,
        .SDcols = cols_lagueables
]

# agrego los delta lags de orden 1
for (vcol in cols_lagueables)
{
  dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
  dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
}

ncol(dataset)
colnames(dataset)

GLOBAL_semilla <- 945799

campos_buenos <- copy( setdiff(
  colnames(dataset), c("clase_ternaria"))
)

set.seed(GLOBAL_semilla, kind = "L'Ecuyer-CMRG")
dataset[, azar:=runif(nrow(dataset))]

dfuture <- dataset[foto_mes==202109]

# undersampling de los CONTINIA al 8%
dataset[, fold_train :=  foto_mes<= 202107 &
          (clase_ternaria %in% c("BAJA+1", "BAJA+2") |
             azar < 0.02 ) ]

dataset[, clase01 := ifelse( clase_ternaria=="CONTINUA", 0, 1 )]

require("xgboost")

# dejo los datos en el formato que necesita LightGBM
dvalidate <- xgb.DMatrix(
  data = data.matrix(dataset[foto_mes==202107, campos_buenos, with = FALSE]),
  label = dataset[foto_mes==202107, clase01]
)


# aqui se hace la magia informatica con los pesos para poder reutilizar
#  el mismo dataset para training y final_train
dtrain <- xgb.DMatrix(
  data = data.matrix(dataset[fold_train == TRUE, campos_buenos, with = FALSE]),
  label = dataset[fold_train == TRUE, clase01],
  weight = dataset[fold_train == TRUE, ifelse( foto_mes<=202106, 1.0, 0.0)]
)

rm( dataset )
gc(full = TRUE, verbose= FALSE) # garbage collection


nrow( dfuture )
nrow( dvalidate )
nrow( dtrain )




EstimarGanancia_AUC_xgboost <- function(x) {
  
  message(format(Sys.time(), "%a %b %d %X %Y"))
  
  param_basicos <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    tree_method = "hist",
    max_bin = 31,
    eta = 0.03,
    colsample_bytree = 0.5
  )
  
  param_train <- list(
    nrounds = 2048, # valor grande, lo limita early_stopping_rounds
    early_stopping_rounds = 200,
    verbose = 0
  )
  
  
  
  param_completo <- c(param_basicos, param_train, x)
  
  modelo_train <- xgb.train(
    data = dtrain,
    evals = list(train = dtrain, eval = dvalidate),
    param = param_basicos,
    nrounds = param_train$nrounds,
    early_stopping_rounds = param_train$early_stopping_rounds,
    verbose = param_train$verbose
  )
  
  
  AUC <- xgb.attributes(modelo_train)$best_score
  
  
  
  
  
  # esta es la forma de devolver un parametro extra
  attr(AUC, "extras") <- list("nrounds" = xgb.attributes(modelo_train)$best_iteration)
  
  
  rm(modelo_train)
  gc(full= TRUE, verbose= FALSE)
  
  return(AUC)
}



# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")<
  
  configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = EstimarGanancia_AUC_xgboost, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando AUC
  noisy = FALSE,
  par.set = makeParamSet(
    makeIntegerParam("max_depth", lower = 3L, upper = 15L), 
    makeNumericParam("min_child_weight", lower = 1, upper = 10)
  ),
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600,
  save.file.path = "lineademuerte.RDATA"
)

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = 10  # cantidad de iteraciones inteligentes
)

# defino el método estandar para la creacion de los puntos iniciales
#   los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# mas configuraciones
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)



if (!file.exists("lineademuerte.RDATA")) {
  bayesiana_salida <<- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  bayesiana_salida <<- mboContinue("lineademuerte.RDATA") # retomo en caso que ya exista
}


tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
setorder(tb_bayesiana, -y, -nrounds) # ordeno en forma descendente por AUC = y
mejores_hiperparametros <- tb_bayesiana[1, # el primero es el de mejor AUC
                                        list(max_depth, min_child_weight, nrounds)]

print(mejores_hiperparametros)

setinfo(dtrain, "weight", rep(1.0, nrow(dtrain)))

param_basicos <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  tree_method = "hist",
  max_bin = 31,
  eta = 0.03,
  colsample_bytree = 0.5
)

param_final <- c(param_basicos, mejores_hiperparametros)

print(param_final)

param_preparado <- list(
  objective = param_final$objective,
  eval_metric = param_final$eval_metric,
  tree_method = param_final$tree_method,
  max_bin = param_final$max_bin,
  eta = param_final$eta,
  colsample_bytree = param_final$colsample_bytree,
  min_child_weight = param_final$min_child_weight,
  max_depth = param_final$max_depth
)



final_model <- xgb.train(
  data = dtrain,
  param = param_preparado,
  nrounds = param_final$nrounds,
  verbose = 0
)


prediccion <- predict(
  final_model,
  data.matrix(dfuture[, campos_buenos, with = FALSE])
)


# genero la tabla de entrega
tb_entrega <- dfuture[, list(numero_de_cliente)]
tb_entrega[, prob := prediccion]

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)
tb_entrega[, prob := NULL] # ya no necesito prob
tb_entrega[, Predicted := 0L]
tb_entrega[1:11000, Predicted := 1L]

fwrite(tb_entrega, file = "lineademuertex_11000.csv" )

format(Sys.time(), "%a %b %d %X %Y")
