rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semilla_primigenia <- 945799
PARAM$qsemillas <- 50

# dataset
PARAM$dataset_nom <- "/kaggle/input/competencia-01b/resultado_con_clase.csv"

PARAM$training_pct <- 70L  # entre  1L y 99L 

PARAM$rpart <- list(
  "cp" = -1, # complejidad minima
  "minsplit" = 700, # minima cantidad de regs en un nodo para hacer el split
  "minbucket" = 350, # minima cantidad de regs en una hoja
  "maxdepth" = 8 # profundidad máxima del arbol
)

#------------------------------------------------------------------------------ 
# particionar agrega una columna llamada fold a un dataset que consiste
#  en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#  crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}
#------------------------------------------------------------------------------ 

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset,
               division = c(param_basicos$training_pct, 100L - param_basicos$training_pct), 
               agrupa = "clase_ternaria",
               seed = semilla # aqui se usa SU semilla
  )

  # genero el modelo
  modelo <- rpart("clase_ternaria ~ .",
                  data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
                  xval = 0,
                  control = param_basicos$rpart
  )

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
                        dataset[fold == 2], # fold==2  es testing, el 30% de los datos
                        type = "prob"
  )

  # calculo la ganancia en testing  que es fold==2
  ganancia_test <- dataset[fold == 2,
                           sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
                                      ifelse(clase_ternaria == "BAJA+2", 273000, -7000),
                                      0))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / ((100 - PARAM$training_pct) / 100)

  return(list(
    "semilla" = semilla,
    "testing" = dataset[fold == 2, .N],
    "testing_pos" = dataset[fold == 2 & clase_ternaria == "BAJA+2", .N],
    "envios" = dataset[fold == 2, sum(prediccion[, "BAJA+2"] > 0.025)],
    "aciertos" = dataset[fold == 2, sum(prediccion[, "BAJA+2"] > 0.025 & clase_ternaria == "BAJA+2")],
    "ganancia_test" = ganancia_test_normalizada
  ))
}
#------------------------------------------------------------------------------ 
#------------------------------------------------------------------------------ 

# Generar números primos
generate_primes <- function(min, max) {
  primes <- c()
  for (num in min:max) {
    if (num > 1) {
      is_prime <- TRUE
      for (i in 2:floor(sqrt(num))) {
        if (num %% i == 0) {
          is_prime <- FALSE
          break
        }
      }
      if (is_prime) {
        primes <- c(primes, num)
      }
    }
  }
  return(primes)
}

# Generar números primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) # inicializo 
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, PARAM$qsemillas)

# cargo los datos
dataset <- fread(PARAM$dataset_nom)

# trabajo, por ahora, solo con 202104
dataset <- dataset[foto_mes == 202104]

# la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
# tantas veces como valores tenga el vector  PARAM$semillas
salidas <- mcmapply(ArbolEstimarGanancia,
                    PARAM$semillas, # paso el vector de semillas
                    MoreArgs = list(PARAM), # aqui paso el segundo parametro
                    SIMPLIFY = FALSE,
                    mc.cores = detectCores()
)

# muestro la lista de las salidas en testing
salidas

# paso la lista a vector
tb_salida <- rbindlist(salidas)

for (i in seq(10, 50, 10)) {
  cat(i, "\t", tb_salida[1:i, mean(ganancia_test)], "\n")
}

cat("desvio : ", tb_salida[, sd(ganancia_test)], "\n")
