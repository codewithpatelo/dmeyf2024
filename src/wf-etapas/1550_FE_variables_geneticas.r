# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full= TRUE, verbose= FALSE) # garbage collection


args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

require( "data.table" )

cn <- envg$PARAM$Creacionismo$cn

if (is.null(cn)) {
  cn <- "0001"
}

k <- envg$PARAM$Creacionismo$k
if (is.null(k)) {
  k <- 1
}


# Especifico carpeta donde guarda el dataset los canarios asesinos.
setwd(paste0("~/buckets/b1/expw/CN-", cn)) # Establezco el Working Directory

# Nombre del dataset
dataset <- fread("dataset.csv.gz")

# Leemos el archivo generado por el componente de canaritos
impo_1 <- fread("impo_1.txt")

# Parámetros del algoritmo genético
num_poblacion_extincion <- 20
num_poblacion_creacion <- 40
prob_cruza <- 1 # Dado que por el momento solo usamos cruza y no mutaciones...
#prob_mutacion <- 0
prob_operadores <- c("+" = 0.25, "-" = 0.25, "*" = 0.25, "/" = 0.25)
prob_atributos <- rep(1 / length(atributos_importantes), length(atributos_importantes))
names(prob_atributos) <- atributos_importantes
#tasa_aprendizaje <- 0.1

# Aplico críterio de Elitismo y selecciono las 20 variables más importantes
atributos_importantes <- impo_1[1:num_poblacion_extincion, Feature] 

# Defino operadores de cruza
operadores_cruza <- c("+", "-", "*", "/")

# Defino operadores de mutación
# Ejem crear medias moviles y trasformaciones de fourrier

# Inicializo poblacion como un data.table vacío
poblacion_atributos <- data.table()

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

# Función para ajustar `prob_cruza` con aprendizaje por refuerzo
ajustar_probabilidades <- function(aptitud_actual, aptitud_anterior, prob_cruza, prob_mutacion, tasa_aprendizaje) {
  recompenza <- aptitud_actual - aptitud_anterior
  prob_cruza <- prob_cruza + tasa_aprendizaje * recompenza
  prob_mutacion <- prob_mutacion + tasa_aprendizaje * recompenza

  # Limitar probabilidades entre 0 y 1
  prob_cruza <- max(min(prob_cruza, 1), 0)
  prob_mutacion <- max(min(prob_mutacion, 1), 0)

  return(list(prob_cruza = prob_cruza, prob_mutacion = prob_mutacion))
}

# Bucle para crear nuevas variables y agregar al dataset
creacion_nueva_generacion <- function(poblacion, num_nuevas) {
  l <- 1
  while (l <= num_nuevas) {
    # Selecciona aleatoriamente dos atributos de la población
    #padres_indices <- sample(1:length(poblacion), 2, prob = prob_atributos, replace = FALSE)
    #padre <- dataset[[poblacion[padres_indices[1]]]]
    #madre <- dataset[[poblacion[padres_indices[2]]]]
    indices <- sample(1:length(poblacion), 2)
    padre <- dataset[[poblacion[indices[1]]]]
    madre <- dataset[[poblacion[indices[2]]]]
    
    # Selecciona un operador aleatorio de la lista
    #operador <- sample(names(prob_operadores), 1, prob = prob_operadores)
    operador <- sample(operadores_cruza, 1)

    nuevo_atributo <- operador_cruza(padre, madre, operador)

    # Creamos nuestro "cromosoma" con el nombre de la nueva columna
    cromosoma <- paste0(k, "_", names(padre)[1], "_", operador, "_", names(madre)[1])
    
    # Agrega la nueva variable al dataset original
    dataset[, (cromosoma) := nuevo_atributo]  
    
    # Agrega al diccionario nuevas_variables
    poblacion_atributos <<- rbind(poblacion_atributos, data.table(nombre = cromosoma))
    
    l <- l + 1  # Incrementa el contador
  }
}

# Llamar a la función para crear la nueva generación
creacion_nueva_generacion(atributos_importantes, num_poblacion_creacion)

# Inicialización de la generación y ajuste de probabilidades
fitness_anterior <- -Inf

for (gen in 1:10) { # Número de generaciones
  # Crear la nueva generación de atributos
  nuevas_columnas <- creacion_nueva_generacion(atributos_importantes, num_poblacion_creacion)

  # Calcular el fitness de la generación actual
  fitness_actual <- calcular_fitness(dataset, nuevas_columnas)
  cat("Generación:", gen, "Fitness:", fitness_actual, "Probabilidad de Cruza:", prob_cruza, "Probabilidad de Mutación:", prob_mutacion, "\n")

  # Ajustar `prob_cruza` y `prob_mutacion` usando RL
  probabilidades <- ajustar_probabilidades(fitness_actual, fitness_anterior, prob_cruza, prob_mutacion, learning_rate)
  prob_cruza <- probabilidades$prob_cruza
  prob_mutacion <- probabilidades$prob_mutacion

  # Actualizar el fitness anterior
  fitness_anterior <- fitness_actual

  # Almacenar las nuevas columnas generadas en esta generación
  poblacion_atributos <<- rbind(poblacion_atributos, data.table(nombre = nuevas_columnas))
}


# Establecer el directorio de trabajo para guardar el archivo
setwd("~/buckets/b1/datasets")

# Grabo las variables
cat("Escritura de variables nuevas\n")
cat("Iniciando grabado de variables nuevas\n")

# Crea el nombre del archivo usando la iteración k
nombre_archivo <- paste0("nuevas_variables_iter_", k, ".csv")

# Guarda el archivo
fwrite(poblacion_atributos, file = nombre_archivo, logical01 = TRUE, sep = ",")
cat("Finalizado grabado de nuevas variables\n")
