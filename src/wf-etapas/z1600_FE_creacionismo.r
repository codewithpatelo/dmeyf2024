# Cargar la librería necesaria
require("data.table")

# Función para recibir argumentos desde la línea de comandos
args <- commandArgs(trailingOnly = TRUE)

# Verificar si se ha pasado el argumento k
if (length(args) < 1) {
  k <- 1  # Valor por defecto para k
} else {
  # Convertir el argumento k a número
  k <- as.numeric(args[1])
}

# Especifico carpeta donde guarda el dataset los canarios asesinos.
setwd("~/buckets/b1/expw/CN-0006") # Establezco el Working Directory

# Nombre del dataset
dataset <- fread("dataset.csv.gz")

# Nombre de la carpeta donde se guardan la importancia de las variables.
setwd("~/buckets/b1/expw/CN-0006")

# Leemos el archivo generado por el componente de canaritos
impo_1 <- fread("impo_1.txt")

# Parámetros del algoritmo genético
num_poblacion_extincion <- 20
num_poblacion_creacion <- 40
prob_cruza <- 1 # Dado que por el momento solo usamos cruza y no mutaciones...

# Aplico críterio de Elitismo y selecciono las 20 variables más importantes
atributos_importantes <- impo_1[1:num_poblacion_extincion, Feature] 

# Defino operadores de cruza
operadores_cruza <- c("+", "-", "*", "/")

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

# Bucle para crear nuevas variables y agregar al dataset
creacion_nueva_generacion <- function(poblacion, num_nuevas) {
  l <- 1
  while (l <= num_nuevas) {
    # Selecciona aleatoriamente dos atributos de la población
    indices <- sample(1:length(poblacion), 2)
    padre <- dataset[[poblacion[indices[1]]]]
    madre <- dataset[[poblacion[indices[2]]]]
    
    # Selecciona un operador aleatorio de la lista
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
