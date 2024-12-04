library("data.table")
library("ggplot2")
library("dplyr")

setwd("C:/Users/POO03/Documents/Patricio/datos_hibridaciones")


kaggle_modelo_1 <- read.table("modelo1_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraCreacionista FeHist1y2 Rf25 Canaritos NoMarzoAbr   
kaggle_modelo_2 <- read.table("modelo2_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraCreacionista FeHist1y2 Rf25 NoCanaritos NoMarzoAbr  
kaggle_modelo_3 <- read.table("modelo3_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraNormal FeHist1y2 Rf25 NoCanaritos NoMarzoAbr 
kaggle_modelo_4 <- read.table("modelo4_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraCreacionismoPlus FeHist1y2 Rf25 NoCanaritos NoMarzoAbr 
kaggle_modelo_5 <- read.table("modelo5_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraCreacionista FeHist1y2 Rf25 NoCanartios NoMarzoAbr xGB
#kaggle_modelo_6 <- read.table("modelo6_ganancias_log.txt", header = TRUE)[c(1:6),] # Linea de la muerte

# Une todos los modelos
kaggle_modelos <- rbind(kaggle_modelo_1, 
                        kaggle_modelo_2, 
                        kaggle_modelo_3,
                        kaggle_modelo_4 ,
                       kaggle_modelo_5
                        )

kaggle_modelos$modelos <- c(rep("modelo_1",6), 
                            rep("modelo_2",6), 
                            rep("modelo_3",6),
                            rep("modelo_4",6) ,
                            rep("modelo_5",6)
                            )

# Me quedo con las variables que me importan: corte, ganancia y modelos
kaggle_modelos <- kaggle_modelos[,c(6,7,9)]

# Promedio de las ganancias de los modelos y los agrego a la tabla
promedio_ganancias <- kaggle_modelos %>%
  group_by(corte) %>%
  summarise(ganancia = mean(ganancia, na.rm = TRUE))
promedio_ganancias$modelos <- rep("promedio", nrow(promedio_ganancias))

kaggle_modelos <- rbind(kaggle_modelos, promedio_ganancias)

#Subir los archivos a Kaggle manualmente

#Poner las ganancias de Kaggle del modelo hibrido
corte <- c(9500, 10000, 10500, 11000, 11500, 12000, 12500)
ganancia_hibrido_1 <- c(127.317, 126.337, 130.817, 132.567, 129.207, 134.177, 132.987)
modelito_1 <- rep("hibrido_1", length(corte))
kaggle_hibrido_1 <- data.frame("corte" = corte, "ganancia" = ganancia_hibrido_1, "modelos" = modelito_1)

ganancia_hibrido_2 <- c(133.197, 134.317, 131.657,  127.037, 131.797, 133.757, 132.847)
modelito_2 <- rep("hibrido_2", length(corte))
kaggle_hibrido_2 <- data.frame("corte" = corte, "ganancia" = ganancia_hibrido_2, "modelos" = modelito_2)

kaggle_modelos <- rbind(kaggle_modelos, kaggle_hibrido_1, kaggle_hibrido_2)

kaggle_modelos <- kaggle_modelos %>%
  group_by(modelos) %>%
  mutate(grosor = case_when(
    ganancia == max(ganancia) ~ 2,  # Línea más gruesa para el modelo con mayor ganancia
    ganancia == min(ganancia) ~ 0.5,  # Línea más delgada para el modelo con menor ganancia
    TRUE ~ 1  # Otros modelos tienen grosor intermedio
  ))

# Usar 'scale_colour_gradient' para asignar colores de acuerdo con la ganancia
ggplot(kaggle_modelos, aes(x = corte, y = ganancia, colour = ganancia, size = grosor)) +
  geom_line() +
  scale_colour_gradient(low = "red", high = "green") +  # Rojo para menor ganancia, verde para mayor ganancia
  theme_minimal() +
  theme(legend.position = "none")  # Opcional: Eliminar leyenda de color