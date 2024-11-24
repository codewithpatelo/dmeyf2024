cat( "ETAPA  z1551_FEev_variables_evolutivas_v5.r  INIT\n")

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

## !IMPORTANTE : PARA EL EXPERIMENTO COLABORATIVO SE USO LA V3
## LA VER3 TIENE MENOS FUNCIONALIDADES (SIN HISTORICO NI ARBOLITOS y SIN LA OPERACION de SUBSTRACCION)

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
AgregaVarRandomForest <- function(GVEZ) {
  
  cat( "inicio AgregaVarRandomForest()\n")
  gc(verbose= FALSE)
  dataset[, clase01 := 0L ]
  dataset[ get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase01_valor1, 
           clase01 := 1L ]
  
  campos_buenos <- setdiff(
    colnames(dataset),
    c( "clase_ternaria", "clase01")
  )
  
  dataset[, entrenamiento :=
            as.integer( get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$training )]
  
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[entrenamiento == TRUE, campos_buenos, with = FALSE]),
    label = dataset[entrenamiento == TRUE, clase01],
    free_raw_data = FALSE
  )
  
  modelo <- lgb.train(
    data = dtrain,
    param = envg$PARAM$lgb_param,
    verbose = -100
  )
  
  cat( "Fin construccion RandomForest\n" )
  # grabo el modelo, achivo .model
  lgb.save(modelo, file="modelo.model" )
  
  qarbolitos <- copy(envg$PARAM$lgb_param$num_iterations)
  
  periodos <- dataset[ , unique( get(envg$PARAM$dataset_metadata$periodo) ) ]
  
  for( periodo in  periodos )
  {
    cat( "periodo = ", periodo, "\n" )
    datamatrix <- data.matrix(dataset[ get(envg$PARAM$dataset_metadata$periodo)== periodo, campos_buenos, with = FALSE])
    
    cat( "Inicio prediccion\n" )
    prediccion <- predict(
      modelo,
      datamatrix,
      type = "leaf"
    )
    cat( "Fin prediccion\n" )
    
    for( arbolito in 1:qarbolitos )
    {
      cat( arbolito, " " )
      hojas_arbol <- unique(prediccion[ , arbolito])
      
      for (pos in 1:length(hojas_arbol)) {
        # el numero de nodo de la hoja, estan salteados
        nodo_id <- hojas_arbol[pos]
        dataset[ get(envg$PARAM$dataset_metadata$periodo)== periodo, paste0(
          GVEZ, "_rf_", sprintf("%03d", arbolito),
          "_", sprintf("%03d", nodo_id)
        ) :=  as.integer( nodo_id == prediccion[ , arbolito]) ]
        
      }
      
      rm( hojas_arbol )
    }
    cat( "\n" )
    
    rm( prediccion )
    rm( datamatrix )
    gc(verbose= FALSE)
  }
  
  gc(verbose= FALSE)
  
  # borro clase01 , no debe ensuciar el dataset
  dataset[ , clase01 := NULL ]
  
}
#------------------------------------------------------------------------------
# Empieza Programa
#------------------------------------------------------------------------------
cat( "ETAPA  z1550_FE_variables_evolutivas_v5.r  START\n")
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

if (envg$PARAM$Creacionismo$canaritos_inicio == TRUE) {
  inicio_key <- paste0("ncol_iter", 0, "_inicio")
  fin_key <- paste0("ncol_iter", 0, "_fin")
  envg$OUTPUT$Creacionismo[[inicio_key]] <- ncol(dataset)
  CanaritosExtincionistas(
    canaritos_ratio = envg$PARAM$Creacionismo$canaritos_ratio,
    canaritos_desvios = envg$PARAM$Creacionismo$canaritos_desvios,
    canaritos_semilla = envg$PARAM$Creacionismo$semilla,
    GVEZ = 0
  )
  
  envg$OUTPUT$Creacionismo[[fin_key]] <- ncol(dataset)
}



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
  Creacionismo_Nueva_Generacion(variables_importantes=variables_importantes, operador="-", k=k) #la v3 no incluia esto
  
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
  
  if (envg$PARAM$Creacionismo$fehist) {
    if (envg$PARAM$lag1) {
      cat("Inicio lag1\n")
      
      # Crear los campos lags de orden 1
      envg$OUTPUT$lag1$ncol_antes <- ncol(dataset)
      dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
              by = eval(envg$PARAM$dataset_metadata$entity_id),
              .SDcols = cols_lagueables]
      
      #la v3 no hacia lags ni delta lags
      # Agregar los delta lags de orden 1 con validación
      for (vcol in cols_lagueables) {
        # Verificar que las columnas son numéricas
        base_col <- dataset[[vcol]]
        lag1_col <- dataset[[paste0(vcol, "_lag1")]]
        
        if (!is.numeric(base_col)) {
          warning(paste("La columna", vcol, "no es numérica. Intentando convertirla."))
          base_col <- suppressWarnings(as.numeric(base_col))
          dataset[[vcol]] <- base_col  # Actualizar la columna original
        }
        
        if (!is.numeric(lag1_col)) {
          warning(paste("La columna", paste0(vcol, "_lag1"), "no es numérica. Intentando convertirla."))
          lag1_col <- suppressWarnings(as.numeric(lag1_col))
          dataset[[paste0(vcol, "_lag1")]] <- lag1_col  # Actualizar la columna lag1
        }
        
        # Validar si la conversión resultó en valores NA
        if (any(is.na(base_col)) || any(is.na(lag1_col))) {
          cat(paste("No se puede calcular el delta: la columna", vcol, "o su lag contiene valores inválidos."))
        }
        
        # Calcular el delta
        dataset[, paste0(vcol, "_delta1") := base_col - lag1_col]
      }
      
      envg$OUTPUT$lag1$ncol_despues <- ncol(dataset)
      GrabarOutput()
      cat("Fin lag1\n")
    }
    
    
    
    # Asegurar que las columnas de lag son válidas
    cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
    
    if (envg$PARAM$lag2) {
      cat("Inicio lag2\n")
      
      # Crear los campos lags de orden 2
      envg$OUTPUT$lag2$ncol_antes <- ncol(dataset)
      dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
              by = eval(envg$PARAM$dataset_metadata$entity_id),
              .SDcols = cols_lagueables]
      
      # Agregar los delta lags de orden 2 con validación
      for (vcol in cols_lagueables) {
        # Validar las columnas originales y lag2
        base_col <- dataset[[vcol]]
        lag2_col <- dataset[[paste0(vcol, "_lag2")]]
        
        if (!is.numeric(base_col)) {
          warning(paste("La columna", vcol, "no es numérica. Intentando convertirla."))
          base_col <- suppressWarnings(as.numeric(base_col))
          dataset[[vcol]] <- base_col  # Actualizar la columna original
        }
        
        if (!is.numeric(lag2_col)) {
          warning(paste("La columna", paste0(vcol, "_lag2"), "no es numérica. Intentando convertirla."))
          lag2_col <- suppressWarnings(as.numeric(lag2_col))
          dataset[[paste0(vcol, "_lag2")]] <- lag2_col  # Actualizar la columna lag2
        }
        
        # Validar si la conversión resultó en valores NA
        if (any(is.na(base_col)) || any(is.na(lag2_col))) {
          cat(paste("No se puede calcular el delta: la columna", vcol, "o su lag2 contiene valores inválidos."))
        }
        
        # Calcular el delta de lag2
        dataset[, paste0(vcol, "_delta2") := base_col - lag2_col]
      }
      
      envg$OUTPUT$lag2$ncol_despues <- ncol(dataset)
      GrabarOutput()
      cat("Fin lag2\n")
    }
  }
  
  if (envg$PARAM$Creacionismo$ferf) {
    # la v3 no incluia RF
    AgregaVarRandomForest(GVEZ=k)
  }
  
  
  
  inicio_key <- paste0("ncol_iter", k, "_inicio")
  fin_key <- paste0("ncol_iter", k, "_fin")
  
  envg$OUTPUT$Creacionismo[[inicio_key]] <- ncol(dataset)
  CanaritosExtincionistas(
    canaritos_ratio = envg$PARAM$Creacionismo$canaritos_ratio,
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

cat( "ETAPA  z1550_FE_variables_evolutivas_v5.r  END\n")