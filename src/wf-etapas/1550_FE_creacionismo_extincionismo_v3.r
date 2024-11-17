cat( "ETAPA  z1551_FE_variables_geneticas.r  INIT\n")

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

#------------------------------------------------------------------------------
# Parametros del Algoritmo Genetico "Reforzado"
#------------------------------------------------------------------------------
cat( "ETAPA  z1550_FE_variables_geneticas.r  START\n")
action_inicializar() 

cat( "Crea ", envg$PARAM$Creacionismo$num_crea,  "\n")

envg$PARAM$Creacionismo$semilla <- envg$PARAM$semilla
semilla <- envg$PARAM$Creacionismo$semilla

num_poblacion_creacion <- envg$PARAM$Creacionismo$num_crea

num_generaciones <- envg$PARAM$Creacionismo$k

prob_cruza <- envg$PARAM$Creacionismo$prob_cruza

prob_mutacion <- envg$PARAM$Creacionismo$prob_mutacion

prob_operadores_cruza <- c("+" = 0.25, "-" = 0.25, "*" = 0.25, "/" = 0.25)
prob_operadores_mutacion <- c("lag1" = 0.25, "lag2" = 0.25, "ventana3" = 0.25, "fourier" = 0.25)

tasa_aprendizaje_poblacion <- envg$PARAM$Creacionismo$tasa_aprendizaje_poblacion

tasa_aprendizaje_atributo <- envg$PARAM$Creacionismo$tasa_aprendizaje_atributo 


# Defino operadores de cruza
operadores_cruza <- c("+", "-", "*", "/")

# Defino operadores de mutación
operadores_mutacion <- c("lag1", "lag2", "ventana3", "fourier")

# Inicializo poblacion como un data.table vacío
nuevos_atributos <- data.table()


param_lgbm <- list(
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  verbosity = -100,
  seed = semilla,
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

#------------------------------------------------------------------------------
# Funciones del Algoritmo Genetico "Reforzado"
#------------------------------------------------------------------------------
# Primero canarito para la extinción
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
  
  set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
  modelo <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalid),
    eval = fganancia_lgbm_meseta,
    param = param_lgbm,
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
  
  cat( "fin CanaritosExtincionistas()\n")
}
#------------------------------------------------------------------------------
# UTILIDADES
#------------------------------------------------------------------------------

atributos_presentes <- function( patributos )
{
  atributos <- unique( patributos )
  comun <- intersect( atributos, colnames(dataset) )
  
  return(  length( atributos ) == length( comun ) )
}

# Función para chequear si las escalas de dos variables son muy distintas
chequear_escala <- function(padre, madre) {
  # Calculamos el rango o desviación estándar para cada variable
  rango_padre <- diff(range(padre, na.rm = TRUE))
  rango_madre <- diff(range(madre, na.rm = TRUE))
  
  # Comparamos los rangos de las dos variables
  if (rango_padre > 10 * rango_madre || rango_madre > 10 * rango_padre) {
    # Si las escalas son muy diferentes, imprimimos un mensaje de advertencia
    cat("¡Advertencia! Las escalas de las variables 'padre' y 'madre' son muy distintas.\n")
    cat("Rango de 'padre':", rango_padre, "\n")
    cat("Rango de 'madre':", rango_madre, "\n")
    cat("La suma de estas variables podría no ser adecuada.\n")
  }
}

es_lageable <- function(atributo) {
  cat("Chequeando si ", atributo, "es lagueable\n")
  # Verifica si el atributo está dentro de las columnas lageables
  if (atributo %in% cols_lagueables) {
    cat(atributo, "es lagueable\n")
    return(TRUE)
  } else {
    cat(atributo, "no es lagueable\n")
    return(FALSE)
  }
}

#------------------------------------------------------------------------------
# FUNCIONES ALGORITMO GENETICO REFORZADO
#------------------------------------------------------------------------------

operador_cruza <- function(padre, madre, operador) {
  # Ejecuta la operación de cruza y calcula el nuevo atributo
  if (operador == "+") {
    nuevo_atributo <- padre + madre
  } else if (operador == "-") {
    nuevo_atributo <- padre - madre
  } else if (operador == "*") {
    nuevo_atributo <- padre * madre
  } else if (operador == "/") {
    nuevo_atributo <- padre / madre
  }
  
  return(nuevo_atributo)
}


operador_mutacion <- function(gen, operador) {
  # Ejecuta la operación de mutacion y calcula el nuevo atributo
  if (operador == "lag1") {
    nuevo_atributo <- shift(gen, n = 1, type = "lag")
  } else if (operador == "lag2") {
    nuevo_atributo <- shift(gen, n = 2, type = "lag")
  } else if (operador == "ventana3") {
    nuevo_atributo <- frollmean(gen, n = 3, align = "right", fill = NA)
  } else if (operador == "fourier") {
    fft_result <- fft(gen)
    nuevo_atributo <- Re(fft_result)
  }
  
  return(nuevo_atributo)
}

# Función de aptitud individual (simple LightGBM)
funcion_aptitud_atributo <- function(nueva_variable, nombre) {
  cat("Inicio evaluación individual para:", nombre, "\n")
  
  # Verifica que el vector nueva_variable tenga la longitud correcta
  if (length(nueva_variable) != nrow(dataset)) {
    stop("La longitud de nueva_variable no coincide con el número de filas de dataset")
  }
  
  # Crear una copia del dataset para evitar modificar el original
  dataset_local <- copy(dataset)
  
  # Generar la clase binaria
  dataset_local[, clase01 := 0L]
  dataset_local[get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase01_valor1, clase01 := 1L]
  
  # Agregar la nueva variable al dataset (ahora con el vector)
  dataset_local[, nueva_var := nueva_variable]
  
  dataset_local[, nueva_var := as.numeric(nueva_variable)]
  
  
  # Seleccionar campos relevantes
  campos_buenos <- setdiff(colnames(dataset_local), c("clase01"))
  campos_buenos <- union(campos_buenos, "nueva_var")
  
  # Crear un dataset LightGBM con una muestra
  set.seed(semilla)
  idx_sample <- sample(1:nrow(dataset_local), size = round(0.1 * nrow(dataset_local)))
  
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset_local[idx_sample, campos_buenos, with = FALSE]),
    label = dataset_local[idx_sample, clase01],
    free_raw_data = FALSE
  )
  
  # Entrenar un modelo muy sencillo
  modelo <- lgb.train(
    data = dtrain,
    param = list(
      objective = "binary",
      learning_rate = 0.1,
      num_leaves = 15,
      min_data_in_leaf = 20,
      feature_fraction = 0.8,
      bagging_fraction = 0.8,
      bagging_freq = 1
    ),
    nrounds = 50,
    verbose = -1
  )
  
  # Obtener la importancia de las variables
  tb_importancia <- lgb.importance(model = modelo)
  
  # Extraer la importancia de la nueva variable de forma segura
  importancia_nueva_var <- ifelse("nueva_var" %in% tb_importancia$Feature,
                                  tb_importancia[Feature == "nueva_var", Gain],
                                  0)
  
  # Normalizar la importancia entre 0 y 1
  max_importancia <- max(tb_importancia$Gain, na.rm = TRUE)
  ratio_importancia <- importancia_nueva_var / max_importancia
  
  cat("Importancia normalizada para", nombre, ":", ratio_importancia, "\n")
  
  return(ratio_importancia)
}




# Función de aptitud global (LightGBM con todas las variables)
funcion_aptitud_poblacion <- function() {
  # Verificar que tb_importancia esté disponible
  if (!exists("tb_importancia")) {
    stop("La tabla 'tb_importancia' no ha sido inicializada. Ejecuta primero 'CanaritosExtincionistas'.")
  }
  
  # Obtener el top 20 de atributos según importancia
  top_20 <- tb_importancia[pos <= 20, Feature]
  
  # Calcular el número de atributos de 'nuevos_atributos' presentes en el top 20
  count_in_top_20 <- sum(nuevos_atributos %in% top_20)
  
  # Normalizar el ratio (número de atributos en top 20 dividido por total de nuevos atributos)
  ratio <- count_in_top_20 / length(nuevos_atributos)
  
  # Retornar ratio normalizado
  cat("Ratio de atributos en el top 20: ", ratio, "\n")
  return(ratio)
}


# Actualización de probabilidades (individual) 
actualizar_probabilidades_atributo <- function(atributo, aptitud, tipo_operador, operador) {
  cat("Actualizando probabilidades para atributo ", atributo, "\n")
  
  
  # Verificar si prob_atributos está correctamente inicializado
  if (is.null(prob_atributos)) {
    # Inicializar prob_atributos con 1 / número de columnas del dataset
    prob_atributos <<- setNames(rep(1 / ncol(dataset), ncol(dataset)), colnames(dataset))
    cat("Probabilidades inicializadas: ", prob_atributos, "\n")
  }
  
  if (!atributo %in% names(prob_atributos)) {
    # Si el atributo no está, agrégalo con probabilidad inicial 0
    prob_atributos <<- c(prob_atributos, setNames(0, atributo))
    cat("Atributo", atributo, "agregado a prob_atributos con probabilidad inicial de: 0\n")
  } else {
    # Si el atributo existe, actualiza su probabilidad
    #prob_atributos[[atributo]] <<- prob_atributos[[atributo]] + aptitud * tasa_aprendizaje_atributo
    #cat("Nueva probabilidad para atributo ", atributo, ": ", prob_atributos[[atributo]], "\n")
  }
  
  # Normalizar las probabilidades
  #prob_atributos <<- prob_atributos / sum(prob_atributos)
  
  
  
  if (tipo_operador == "cruza") {
    # Verificar e inicializar prob_operadores_cruza si es necesario
    if (is.null(prob_operadores_cruza)) {
      prob_operadores_cruza <<- rep(0, length(operadores_cruza))  # Ajustar según la longitud
    }
    
    # Actualizar probabilidad global de cruza
    prob_cruza <<- prob_cruza + 0.1 * aptitud
    
    # Actualizar probabilidad de los operadores de cruza
    prob_operadores_cruza[[operador]] <<- prob_operadores_cruza[[operador]] + 0.1 * aptitud
    prob_operadores_cruza <<- prob_operadores_cruza / sum(prob_operadores_cruza)  # Normaliza
    
    cat("Nueva probabilidad de cruza: ", prob_cruza, "\n")
    cat("Nueva probabilidad de operadores de cruza: ", prob_operadores_cruza, "\n")
    
  } else if (tipo_operador == "mutación") {
    # Verificar e inicializar prob_operadores_mutacion si es necesario
    if (is.null(prob_operadores_mutacion)) {
      prob_operadores_mutacion <<- rep(0, length(operadores_mutacion))  # Ajustar según la longitud
    }
    
    # Actualizar probabilidad global de mutación
    prob_mutacion <<- prob_mutacion + 0.1 * aptitud
    
    # Actualizar probabilidad de los operadores de mutación
    prob_operadores_mutacion[[operador]] <<- prob_operadores_mutacion[[operador]] + 0.1 * aptitud
    prob_operadores_mutacion <<- prob_operadores_mutacion / sum(prob_operadores_mutacion)  # Normaliza
    
    cat("Nueva probabilidad de mutación: ", prob_mutacion, "\n")
    cat("Nueva probabilidad de operadores de mutación: ", prob_operadores_mutacion, "\n")
  }
  
  # Normalizar las probabilidades globales de cruza y mutación
  total <- prob_cruza + prob_mutacion
  prob_cruza <<- prob_cruza / total
  prob_mutacion <<- prob_mutacion / total
  
  cat("Probabilidades normalizadas: Cruza = ", prob_cruza, ", Mutación = ", prob_mutacion, "\n")
}



actualizar_probabilidades_poblacion <- function(aptitud_actual, aptitud_anterior) {
  cat( "Actualizando probabilidades... \n")
  recompenza <- aptitud_actual - aptitud_anterior
  prob_cruza <- prob_cruza + tasa_aprendizaje_poblacion * recompenza
  prob_mutacion <- prob_mutacion + tasa_aprendizaje_poblacion * recompenza
  
  # Normaliza las probabilidades de cruza y mutación
  total <- prob_cruza + prob_mutacion
  prob_cruza <<- prob_cruza / total
  prob_mutacion <<- prob_mutacion / total
}

# Bucle para crear nuevas variables y agregar al dataset
Creacion_Nueva_Generacion <- function() {
  cat( "Inicio Creacion_Nueva_Generacion()\n")
  
  
  num_vars_inicio = ncol(dataset)
  if (num_vars_inicio > num_poblacion_creacion) {
    vars_a_crear = 1
  } else {
    vars_a_crear = num_poblacion_creacion - num_vars_inicio
  }
  
  for (l in 1:vars_a_crear) {
    exito <- FALSE
    cat( "Var -> ", l, "\n")
    cat("Generando nueva variable, total actuales:", ncol(dataset), "\n")
    cat("Objetivo de creación: ",l, "/", vars_a_crear, "(", num_vars_inicio+vars_a_crear, "/", num_vars_inicio ")\n")
    
    # Selecciona el tipo de operador según las probabilidades prob_cruza y prob_mutacion
    cat("Selecionando tipo de operador...\n")
    tipo_operador <- sample(c("cruza", "mutacion"), 1, prob = c(prob_cruza, prob_mutacion))
    cat("Tipo operador:", tipo_operador, "\n")
    
    
    if (tipo_operador == "cruza") {
      cat("Seleccionando atributos...\n")
      # Selecciona aleatoriamente dos atributos de la población
      indices <- sample(1:ncol(dataset), 2, prob = prob_atributos, replace = FALSE)
      padre <- dataset[[indices[1]]]
      madre <- dataset[[indices[2]]]
      padre_nombre <- names(dataset)[indices[1]]
      madre_nombre <- names(dataset)[indices[2]]
      cat(padre_nombre, " y ", madre_nombre, "\n")
      
      
      # Selecciona un operador de cruza según sus probabilidades
      cat("Seleccionando operador...\n")
      operador <- sample(names(prob_operadores_cruza), 1, prob = prob_operadores_cruza)
      cat("Operador:", operador, "\n")
      
      # Realiza la operación de cruza
      # Crea el "cromosoma" para cruza
      cromosoma <- paste0(k, "_", padre_nombre, "_", operador, "_", madre_nombre)
      if( atributos_presentes( c(padre_nombre, madre_nombre) )) {
        cat("Cruzando: ", padre_nombre, "y ", madre_nombre, "\n")
        
        if (!is.numeric(padre) || !is.numeric(madre)) {
          cat("'padre' y 'madre' deben ser valores numéricos. Abortando cruza.")
          exito <- FALSE
        } else {
          if (operador == "+") {
            chequear_escala(padre, madre)
          }
          nuevo_atributo <- operador_cruza(padre, madre, operador)
          dataset[, paste0("iter_",k,"_var_",l) := nuevo_atributo]
          
          # Agrega al diccionario nuevos_atributos
          nuevos_atributos <<- rbind(nuevos_atributos, data.table(nombre = paste0("iter_",k,"_var_",l),
                                                                  explicacion = cromosoma))
          cat("Atributo: ", cromosoma, "\n")
          exito <- TRUE
        }
        
        
        
        
      } else {
        cat("Atributos no presentes. Abortando operación.\n")
        exito <- FALSE
      }
      
      
      
    } else if (tipo_operador == "mutacion") {
      # Selecciona aleatoriamente un atributo de la población
      cat("Seleccionando atributo...\n")
      indice <- sample(1:ncol(dataset), 1, prob = prob_atributos)
      gen <- dataset[[indice]]  # Obtiene la columna seleccionada
      gen_nombre <- names(dataset)[indice]
      cat("Seleccionado: ", names(dataset)[indice], "\n")  # Imprime el nombre de la columna
      
      
      if(!es_lageable(names(dataset)[indice])) {
        cat("Este atributo no es lagueable. Abortando mutación.\n")
        exito <- FALSE
      } else {
        # Selecciona un operador de mutación según sus probabilidades
        cat("Seleccionando operador...\n")
        operador <- sample(names(prob_operadores_mutacion), 1, prob = prob_operadores_mutacion)
        cat("Operador:", operador, "\n")
        
        if( atributos_presentes( c(gen_nombre ))) {
          # Realiza la operación de mutación
          cromosoma <- paste0(k, "_", gen_nombre, "_", operador)
          nuevo_atributo <- operador_mutacion(gen, operador)
          dataset[, paste0("iter_",k,"_var_",l) := nuevo_atributo]
          
          # Agrega al diccionario nuevos_atributos
          nuevos_atributos <<- rbind(nuevos_atributos, data.table(nombre = paste0("iter_",k,"_var_",l),
                                                                  explicacion = cromosoma))
          cat("Atributo: ", cromosoma, "\n")
          exito <- TRUE
        } else {
          cat("Atributo no presente. Abortando operación.\n")
        }
        
      }
      
      
    }
    
    # Realiza la evaluación de aptitud de la nueva variable
    if (exito == TRUE && envg$PARAM$Creacionismo$reforzado == TRUE ) {
      cat("Calculando aptitud nueva variable...\n")
      aptitud <- funcion_aptitud_atributo(nuevo_atributo, cromosoma)
      cat("Actualizando probabilidades...\n")
      prob_atributos <- rep(1 / length(dataset), length(dataset))
      actualizar_probabilidades_atributo(paste0("iter_",k,"_var_",l), aptitud, tipo_operador, operador)
    }
    
    
    
    l <- l + 1  # Incrementa el contador
  }
  cat( "Fin Creacion_Nueva_Generacion()\n")
}

#------------------------------------------------------------------------------
# Aqui comienza el programa
# cargo el dataset
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
envg$OUTPUT$Creacionismo$ncol_iter0 <- ncol(dataset)
#--------------------------------------------------------------------------

prob_atributos <<- setNames(rep(1 / ncol(dataset), ncol(dataset)), colnames(dataset))


cat( "Variables creacionistas geneticas\n")
# Inicialización de la generación y ajuste de probabilidades
aptitud_anterior_poblacion <<- 0
set.seed(semilla)

for (k in 1:num_generaciones) { # Número de generaciones
  cat( "Inicio Creacionismo de Generacion nro ", k, "\n")
  nuevos_atributos <<- data.table()
  # Llamar a la función para crear la nueva generación
  Creacion_Nueva_Generacion()
  
  cat( "Escritura de variables nuevas\n")
  cat( "Iniciando grabado de variables nuevas\n" )
  
  nombre_archivo <- paste0("nuevas_variables_iter_", k, ".txt")
  
  # Guarda el archivo
  fwrite(nuevos_atributos, file = nombre_archivo, logical01 = TRUE, sep = ",")
  cat( "Finalizado grabado de nuevas variables\n" )
  
  #--------------------------------------------------------------------------
  
  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )
  
  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }
  
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
  
  
  
  # Elimino las variables que no son tan importantes en el dataset
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
  
  
  cat("Actualizando longuitud de prob_atributos...")
  prob_atributos <<- setNames(rep(1 / ncol(dataset), ncol(dataset)), colnames(dataset))
  cat("Longuitud probs: ",length(prob_atributos), "Longuitud dataset: ", ncol(dataset))
  #--------------------------------------------------------------------------
  # Calcular el fitness de la generación actual
  if(envg$PARAM$Creacionismo$reforzado == TRUE) {
    aptitud_actual_poblacion <<- funcion_aptitud_poblacion()
    cat("Generación:", k, "Aptitud:", aptitud_actual_poblacion, "Probabilidad de Cruza:", prob_cruza, "Probabilidad de Mutación:", prob_mutacion, "\n")
    aptitud_key <- paste0("iter", k, "_aptitud")
    envg$OUTPUT$Creacionismo[[aptitud_key]] <- aptitud_actual_poblacion
    
    # Ajustar probabilidades usando RL
    actualizar_probabilidades_poblacion(aptitud_actual_poblacion, aptitud_anterior_poblacion)
    
    # Actualizar el fitness anterior
    aptitud_anterior_poblacion <<- aptitud_actual_poblacion
    
  }
  
  GrabarOutput()
}

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

cat( "ETAPA  z1550_FE_variables_geneticas.r  END\n")