cat( "ETAPA  z1551_FE_variables_geneticas.r  INIT\n")

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full= TRUE, verbose= FALSE) # garbage collection


args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

require( "data.table" )
require("yaml", quietly=TRUE)

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Parametros del Algoritmo Genetico "Reforzado"
#------------------------------------------------------------------------------
evg$PARAM$Creacionismo$semilla <- envg$PARAM$semilla
  
semilla <- envg$PARAM$Creacionismo$semilla
num_poblacion_extincion <- ifelse(!is.null(envg$PARAM$Creacionismo$num_ext) && !is.na(envg$PARAM$Creacionismo$num_ext), 
                                  envg$PARAM$Creacionismo$num_ext, 
                                  600)
num_poblacion_creacion <- ifelse(!is.null(envg$PARAM$Creacionismo$num_crea) && !is.na(envg$PARAM$Creacionismo$num_crea), 
                                  envg$PARAM$Creacionismo$num_ext, 
                                  800)
num_generaciones <- ifelse(!is.null(envg$PARAM$Creacionismo$k) && !is.na(envg$PARAM$Creacionismo$k), 
                                  envg$PARAM$Creacionismo$k, 
                                  10)
prob_cruza <- ifelse(!is.null(envg$PARAM$Creacionismo$prob_cruza) && !is.na(envg$PARAM$Creacionismo$prob_cruza), 
                                  envg$PARAM$Creacionismo$prob_cruza, 
                                  0.9) 
prob_mutacion <- ifelse(!is.null(envg$PARAM$Creacionismo$prob_mutacion) && !is.na(envg$PARAM$Creacionismo$prob_mutacion), 
                                  envg$PARAM$Creacionismo$prob_mutacion, 
                                  0.1) 
prob_operadores_cruza <- c("+" = 0.25, "-" = 0.25, "*" = 0.25, "/" = 0.25)
prob_operadores_mutacion <- c("lag1" = 0.25, "lag2" = 0.25, "ventana3" = 0.25, "fourier" = 0.25)

tasa_aprendizaje_poblacion <- ifelse(!is.null(envg$PARAM$Creacionismo$tasa_aprendizaje) && !is.na(envg$PARAM$Creacionismo$tasa_aprendizaje), 
                                  envg$PARAM$Creacionismo$tasa_aprendizaje_poblacion, 
                                  0.2) 

tasa_aprendizaje_atributo <- ifelse(!is.null(envg$PARAM$Creacionismo$tasa_aprendizaje) && !is.na(envg$PARAM$Creacionismo$tasa_aprendizaje), 
                                  envg$PARAM$Creacionismo$tasa_aprendizaje_atributo, 
                                  0.1) 


# Defino operadores de cruza
operadores_cruza <- c("+", "-", "*", "/")

# Defino operadores de mutación
operadores_mutacion <- c("lag1", "lag2", "ventana3", "fourier")

# Inicializo poblacion como un data.table vacío
poblacion_atributos <- data.table()


param_lgbm <- list(
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
    median(pos) + canaritos_desvios *0 sd(pos)
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

es_lageable <- function(atributo, cols_lageables) {
  # Verifica si el atributo está dentro de las columnas lageables
  if (atributo %in% cols_lageables) {
    return(TRUE)
  } else {
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
funcion_aptitud_atributo <- function(nueva_variable) {
  cat("Inicio evaluación individual para:", nueva_variable, "\n")
  
  # Crear una copia del dataset para evitar modificar el original
  dataset_local <- copy(dataset)
  
  # Generar la clase binaria
  dataset_local[, clase01 := 0L]
  dataset_local[get(clase) %in% envg$PARAM$train$clase01_valor1, clase01 := 1L]
  
  # Agregar la nueva variable al dataset
  dataset_local[, nueva_var := get(nueva_variable)]
  
  # Seleccionar campos relevantes
  campos_buenos <- setdiff(colnames(dataset_local), c("clase01"))
  
  # Crear un dataset LightGBM con una muestra
  set.seed(semilla)
  idx_sample <- sample(1:nrow(dataset_local), size = min(5000, nrow(dataset_local)))
  
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
  
  # Extraer la importancia de la nueva variable
  importancia_nueva_var <- tb_importancia[Feature == "nueva_var", Gain]
  if (is.na(importancia_nueva_var)) importancia_nueva_var <- 0
  
  # Normalizar la importancia entre 0 y 1
  max_importancia <- max(tb_importancia$Gain, na.rm = TRUE)
  ratio_importancia <- importancia_nueva_var / max_importancia
  
  cat("Importancia normalizada para", nueva_variable, ":", ratio_importancia, "\n")
  
  return(ratio_importancia)
}



# Función de aptitud global (LightGBM con todas las variables)
funcion_aptitud_poblacion <- function(nuevos_atributos) {
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
  cat("Actualizando probabilidades para atributo ", names(atributo)[1], "\n")
  
  # Actualización de probabilidades del atributo
  prob_atributos[[atributo]] <<- prob_atributos[[atributo]] + aptitud * 0.1
  prob_atributos <<- prob_atributos / sum(unlist(prob_atributos))  # Normaliza
  cat("Nueva probabilidad para atributo ", names(atributo)[1], ": ", prob_atributos[[atributo]], "\n")
  
  if (tipo_operador == "cruza") {
    # Actualizar probabilidad global de cruza
    prob_cruza <<- prob_cruza + 0.1 * aptitud
    
    # Actualizar probabilidad de los operadores de cruza
    prob_operadores_cruza[[operador]] <<- prob_operadores_cruza[[operador]] + 0.1 * aptitud
    prob_operadores_cruza <<- prob_operadores_cruza / sum(unlist(prob_operadores_cruza))  # Normaliza
    
    cat("Nueva probabilidad de cruza: ", prob_cruza, "\n")
    cat("Nueva probabilidad de operadores de cruza: ", prob_operadores_cruza, "\n")
    
  } else if (tipo_operador == "mutación") {
    # Actualizar probabilidad global de mutación
    prob_mutacion <<- prob_mutacion + 0.1 * aptitud
    
    # Actualizar probabilidad de los operadores de mutación
    prob_operadores_mutacion[[operador]] <<- prob_operadores_mutacion[[operador]] + 0.1 * aptitud
    prob_operadores_mutacion <<- prob_operadores_mutacion / sum(unlist(prob_operadores_mutacion))  # Normaliza
    
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
  cat( "Actualizando probabilidades...")
  recompenza <- aptitud_actual - aptitud_anterior
  prob_cruza <- prob_cruza + tasa_aprendizaje * recompenza
  prob_mutacion <- prob_mutacion + tasa_aprendizaje * recompenza

  # Normaliza las probabilidades de cruza y mutación
  total <- prob_cruza + prob_mutacion
  prob_cruza <<- prob_cruza / total
  prob_mutacion <<- prob_mutacion / total
}

# Bucle para crear nuevas variables y agregar al dataset
Creacion_Nueva_Generacion <- function() {
  cat( "Inicio Creacion_Nueva_Generacion()\n")

  set.seed(semilla)
 
  # hasta que dataset length tenga el valor de num_poblacion_creacion

  num_vars <- num_poblacion_creacion - ncol(dataset)
  if (num_vars < 0) {
     num_vars <- 1
  }

  for (l in 1:num_vars) {
    cat( "Var -> ", l)
    cat("Generando nueva variable, total actuales:", ncol(dataset), "\n")

     # Selecciona el tipo de operador según las probabilidades prob_cruza y prob_mutacion
    cat("Selecionando tipo de operador:")
    tipo_operador <- sample(c("cruza", "mutacion"), 1, prob = c(prob_cruza, prob_mutacion))
    cat("Tipo operador:", tipo_operador, "\n")

    
    if (tipo_operador == "cruza") {
      cat("Seleccionando atributos...")
      # Selecciona aleatoriamente dos atributos de la población
      indices <- sample(1:length(dataset), 2, prob = prob_atributos, replace = FALSE)
      padre <- dataset[indices[1]]
      madre <- dataset[indices[2]]
      cat(names(padre)[1], " y ", names(madre)[1])
      
      # Selecciona un operador de cruza según sus probabilidades
      cat("Seleccionando operador...")
      operador <- sample(names(prob_operadores_cruza), 1, prob = prob_operadores_cruza)
      cat("Operador:", operador, "\n")
      
      # Realiza la operación de cruza
      # Crea el "cromosoma" para cruza
      cromosoma <- paste0(k, "_", names(padre)[1], "_", operador, "_", names(madre)[1])
      if( atributos_presentes( c(names(padre)[1], names(madre)[1]) )) {
        dataset[, cromosoma := operador_cruza(padre, madre, operador)]

        # Agrega al diccionario nuevos_atributos
        nuevos_atributos <- rbind(nuevos_atributos, data.table(nombre = colname,
                                                           explicacion = cromosoma))
        cat("Atributo: ", cromosoma, "\n")
      } else {
        cat("Atributos no presentes. Abortando operación.")
      }
      
      

    } else if (tipo_operador == "mutacion") {
      # Selecciona aleatoriamente un atributo de la población
      cat("Seleccionando atributo...")
      indice <- sample(1:length(poblacion), 1, prob = prob_atributos)
      gen <- dataset[indice]
      cat(names(gen)[1])

      if(!es_lageable(gen)) {
        cat("Este atributo no es lageable. Abortando mutación.")
      } else {
        # Selecciona un operador de mutación según sus probabilidades
        cat("Seleccionando operador...")
        operador <- sample(names(prob_operadores_mutacion), 1, prob = prob_operadores_mutacion)
        cat("Operador:", operador, "\n")
      
        if( atributos_presentes( c(names(gen)[1] ))) {
           # Realiza la operación de mutación
           cromosoma <- paste0(k, "_", names(gen)[1], "_", operador)
           dataset[, cromosoma := operador_mutacion(gen, operador)]

           # Agrega al diccionario nuevos_atributos
           nuevos_atributos <- rbind(nuevos_atributos, data.table(nombre = colname,
                                                           explicacion = cromosoma))
           cat("Atributo: ", cromosoma, "\n")
        } else {
          cat("Atributo no presentes. Abortando operación.")
        }

      }
      

    }

    # Realiza la evaluación de aptitud de la nueva variable
    cat("Calculando aptitud nueva variable...")
    aptitud <- funcion_aptitud_individual(cromosoma)

    cat("Actualizando probabilidades...")
    actualizar_probabilidades_atributo(cromosoma, aptitud)
    
    
    l <- l + 1  # Incrementa el contador
  }
  cat( "Fin Creacion_Nueva_Generacion()\n")
}

#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "ETAPA  z1550_FE_variables_geneticas.r  START\n")
action_inicializar() 
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

prob_atributos <- rep(1 / length(dataset), length(dataset))


cat( "variables creacionistas geneticas\n")
# Inicialización de la generación y ajuste de probabilidades
aptitud_anterior_poblacion <- -Inf

for (k in 1:num_generaciones) { # Número de generaciones
  cat( "Inicio Creacionismo de Generacion nro ", k)
  poblacion_atributos <- data.table()
  # Llamar a la función para crear la nueva generación
  Creacion_Nueva_Generacion()

  cat( "escritura de variables nuevas\n")
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
    canaritos_ratio = envg$PARAM$Creacionismo$ratio_canaritos,
    canaritos_desvios = envg$PARAM$Creacionismo$desvios_canaritos,
    canaritos_semilla = envg$PARAM$Creacionismo$semilla,
    GVEZ = k
  )
  envg$OUTPUT$Creacionismo[[fin_key]] <- ncol(dataset)
  #--------------------------------------------------------------------------
  # Calcular el fitness de la generación actual
   
  aptitud_actual_poblacion <- funcion_aptitud_poblacion()
  cat("Generación:", k, "Aptitud:", aptitud_actual_poblacion, "Probabilidad de Cruza:", prob_cruza, "Probabilidad de Mutación:", prob_mutacion, "\n")
  aptitud_key <- paste0("iter", k, "_aptitud")
  envg$OUTPUT$Creacionismo[[aptitud_key]] <- aptitud_actual_poblacion

  # Ajustar probabilidades usando RL
  probabilidades <- actualizar_probabilidades_poblacion(aptitud_actual_poblacion, aptitud_anterior_poblacion)

  # Actualizar el fitness anterior
  aptitud_anterior_poblacion <- aptitud_actual_poblacion
  GrabarOutput()



#------------------------------------------------------------------------------

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