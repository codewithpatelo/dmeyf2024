cat( "ETAPA  z1551_FEev_variables_evolutivas_v3.r  INIT\n")

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full= TRUE, verbose= FALSE) # garbage collection


args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

require( "data.table" )
require("yaml", quietly=TRUE)
require("Rcpp", quietly=TRUE)
require("lightgbm", quietly=TRUE)

#------------------------------------------------------------------------------
# FUNCIONES CREACIONISMO
#------------------------------------------------------------------------------

Creacionismo_Nueva_Generacion <- function (variables_importantes, operador, k) {
  l=1
  for (i in 1:length(variables_importantes)) {
    for (j in i:length(variables_importantes)) {
      cat("Cruzando: ", variables_importantes[i], "y ", variables_importantes[j], "\n")
      cat("Operador:", operador, "\n")
      if (operador == "+") {
        nueva_variable <- dataset[[variables_importantes[i]]] + dataset[[variables_importantes[j]]]
      } else if (operador == "-") {
        nueva_variable <- dataset[[variables_importantes[i]]] - dataset[[variables_importantes[j]]]
      } else if (operador == "*") {
        nueva_variable <- dataset[[variables_importantes[i]]] * dataset[[variables_importantes[j]]]
      } else if (operador == "/") {
        nueva_variable <- dataset[[variables_importantes[i]]] / dataset[[variables_importantes[j]]]
      }
      
      # Crea el nombre de la nueva columna
      colname <- paste0("iter_",k,"_var_",l)
      l=l+1
      # Agrega la nueva variable al dataset original
      dataset[, (colname) := nueva_variable]
      # Agrega al diccionario nuevas_variables
      if (operador == "+") {
              nuevas_variables <<- rbind(nuevas_variables, data.table(nombre = colname,
                                                           explicacion = paste0(variables_importantes[i], "_+_", variables_importantes[j])))
      } else if (operador == "-") {
              nuevas_variables <<- rbind(nuevas_variables, data.table(nombre = colname,
                                                           explicacion = paste0(variables_importantes[i], "_-_", variables_importantes[j])))
      } else if (operador == "*") {
              nuevas_variables <<- rbind(nuevas_variables, data.table(nombre = colname,
                                                           explicacion = paste0(variables_importantes[i], "_x_", variables_importantes[j])))
      } else if (operador == "/") {
              nuevas_variables <<- rbind(nuevas_variables, data.table(nombre = colname,
                                                           explicacion = paste0(variables_importantes[i], "_/_", variables_importantes[j])))
      }

    }
  }

}



# FUNCIONES CANARITOS
#------------------------------------------------------------------------------
VPOS_CORTE <- c()

fganancia_lgbm_meseta <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")

  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1, envg$PARAM$train$gan1, envg$PARAM$train$gan0)
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  setorder(tbl, -gan_acum) # voy por la meseta

  gan <- mean(tbl[1:500, gan_acum]) # meseta de tamaño 500

  pos_meseta <- tbl[1:500, median(posicion)]
  VPOS_CORTE <<- c(VPOS_CORTE, pos_meseta)

  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------
# Elimina del dataset las variables que estan por debajo
#  de la capa geologica de canaritos
# se llama varias veces, luego de agregar muchas variables nuevas,
#  para ir reduciendo la cantidad de variables
# y así hacer lugar a nuevas variables importantes

CanaritosExtincionistas <- function(
  canaritos_ratio,
  canaritos_desvios,
  canaritos_semilla,
  GVEZ) {

  cat( "inicio CanaritosAsesinos()\n")
  gc(verbose= FALSE)
  dataset[, clase01 := 0L ]
  dataset[ get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase01_valor1, 
      clase01 := 1L ]

  set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
  for (i in 1:(ncol(dataset) * canaritos_ratio)) {
    dataset[, paste0("canarito", i) := runif(nrow(dataset))]
  }

  campos_buenos <- setdiff(
    colnames(dataset),
    c( campitos, "clase01")
  )

  azar <- runif(nrow(dataset))

  dataset[, entrenamiento :=
    as.integer( get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$training &
      (clase01 == 1 | azar < envg$PARAM$train$undersampling))]

  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[entrenamiento == TRUE, campos_buenos, with = FALSE]),
    label = dataset[entrenamiento == TRUE, clase01],
    weight = dataset[
      entrenamiento == TRUE,
      ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, 1.0000001, 1.0)
    ],
    free_raw_data = FALSE
  )

  dvalid <- lgb.Dataset(
    data = data.matrix(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$validation, campos_buenos, with = FALSE]),
    label = dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$validation, clase01],
    weight = dataset[
      get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$validation,
      ifelse( get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, 1.0000001, 1.0)
    ],
    free_raw_data = FALSE
  )


  param <- list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    seed = canaritos_semilla,
    max_depth = -1, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # por ahora, lo dejo fijo
    lambda_l1 = 0.0, # por ahora, lo dejo fijo
    lambda_l2 = 0.0, # por ahora, lo dejo fijo
    max_bin = 31, # por ahora, lo dejo fijo
    num_iterations = 9999, # un numero grande, lo limita early_stopping_rounds
    force_row_wise = TRUE, # para que los alumnos no se atemoricen con  warning
    learning_rate = 0.065,
    feature_fraction = 1.0, # lo seteo en 1
    min_data_in_leaf = 260,
    num_leaves = 60,
    early_stopping_rounds = 200,
    num_threads = 1
  )

  set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
  modelo <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalid),
    eval = fganancia_lgbm_meseta,
    param = param,
    verbose = -100
  )

  tb_importancia <<- lgb.importance(model = modelo)
  tb_importancia[, pos := .I]

  fwrite(tb_importancia,
    file = paste0("impo_", GVEZ, ".txt"),
    sep = "\t"
  )


  umbral <- tb_importancia[
    Feature %like% "canarito",
    median(pos) + canaritos_desvios * sd(pos)
  ] # Atencion corto en la mediana mas desvios!!

  col_utiles <- tb_importancia[
    pos < umbral & !(Feature %like% "canarito"),
    Feature
  ]

  col_utiles <- unique(c(
    col_utiles,
    c(campitos, "mes")
  ))

  col_inutiles <- setdiff(colnames(dataset), col_utiles)

  dataset[, (col_inutiles) := NULL]

  cat( "fin CanaritosAsesinos()\n")
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Empieza Programa
#------------------------------------------------------------------------------
cat( "ETAPA  z1550_FE_variables_evolutivas_v3.r  START\n")
action_inicializar() 

num_generaciones <- envg$PARAM$Creacionismo$k

envg$PARAM$Creacionismo$semilla <- envg$PARAM$semilla
semilla <- envg$PARAM$Creacionismo$semilla
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

colnames(dataset)[which(!(sapply(dataset, typeof) %in% c("integer", "double")))]

GrabarOutput()

#--------------------------------------
# estas son las columnas a las que se puede agregar
#  lags o media moviles ( todas menos las obvias )

campitos <- c( envg$PARAM$dataset_metadata$primarykey,
  envg$PARAM$dataset_metadata$entity_id,
  envg$PARAM$dataset_metadata$periodo,
  envg$PARAM$dataset_metadata$clase )

campitos <- unique( campitos )

cols_lagueables <- copy(setdiff(
  colnames(dataset),
  envg$PARAM$dataset_metadata
))

# ordeno el dataset por primary key
#  es MUY  importante esta linea
# ordeno dataset
cat( "ordenado del dataset\n")
setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# Elimino las variables que no son tan importantes en el dataset
# with great power comes grest responsability
#ACA HAY QUE EMPEZAR UN MEGA BUCLE
k <- 1

  inicio_key <- paste0("ncol_iter", k, "_inicio")
  fin_key <- paste0("ncol_iter", k, "_fin")
  envg$OUTPUT$Creacionismo[[inicio_key]] <- ncol(dataset)
  CanaritosExtincionistas(
      canaritos_ratio = 0.2,
      canaritos_desvios = envg$PARAM$Creacionismo$canaritos_desvios,
      canaritos_semilla = envg$PARAM$Creacionismo$semilla,
      GVEZ = k
  )

  envg$OUTPUT$Creacionismo[[fin_key]] <- ncol(dataset)



#------------------------------------------------------------------------------

for (k in 1:num_generaciones) {
  cat("Inicio Creacionismo de Generacion nro", k, "\n")
  impo <- tb_importancia
  variables_importantes <- impo[1:20, Feature] #Selecciono las 20 variables mas importantes
  #AQUI COMIENZO A CREAR NUEVAS VARIABLES-----------------------------------------

  nuevas_variables <<- data.table()

  Creacionismo_Nueva_Generacion(variables_importantes=variables_importantes, operador="+", k=k)
  Creacionismo_Nueva_Generacion(variables_importantes=variables_importantes, operador="*", k=k)
  Creacionismo_Nueva_Generacion(variables_importantes=variables_importantes, operador="/", k=k)

  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )
  
  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )
  }
  

  # grabo las variables
  cat( "escritura de variables nuevas\n")
  cat( "Iniciando grabado de variables nuevas\n" )
  # Crea el nombre del archivo usando la iteración k
  nombre_archivo <- paste0("nuevas_variables_iter_", k, ".txt")

  # Guarda el archivo
  fwrite(nuevas_variables, file = nombre_archivo, logical01 = TRUE, sep = ",")
  cat( "Finalizado grabado de nuevas variables\n" )

  inicio_key <- paste0("ncol_iter", k, "_inicio")
  fin_key <- paste0("ncol_iter", k, "_fin")

  envg$OUTPUT$Creacionismo[[inicio_key]] <- ncol(dataset)
  CanaritosExtincionistas(
      canaritos_ratio = 0.2,
      canaritos_desvios = envg$PARAM$Creacionismo$canaritos_desvios,
      canaritos_semilla = envg$PARAM$Creacionismo$semilla,
      GVEZ = k
  )

  envg$OUTPUT$Creacionismo[[fin_key]] <- ncol(dataset)

}

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

cat( "ETAPA  z1550_FE_variables_evolutivas_v3.r  END\n")