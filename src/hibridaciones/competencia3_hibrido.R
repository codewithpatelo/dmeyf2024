library("data.table")
library("ggplot2")
library("dplyr")

setwd("C:/Users/POO03/Documents/Patricio/datos_hibridaciones")



# EDA de ganancias de kaggle----

kaggle_modelo_1 <- read.table("modelo1_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraCreacionista FeHist1y2 Rf25 Canaritos NoMarzoAbr   
kaggle_modelo_2 <- read.table("modelo2_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraCreacionista FeHist1y2 Rf25 NoCanaritos NoMarzoAbr  
kaggle_modelo_3 <- read.table("modelo3_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraNormal FeHist1y2 Rf25 NoCanaritos NoMarzoAbr 
kaggle_modelo_4 <- read.table("modelo4_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraCreacionismoPlus FeHist1y2 Rf25 NoCanaritos NoMarzoAbr 
kaggle_modelo_5 <- read.table("modelo5_ganancias_log.txt", header = TRUE)[c(1:6),] # IntraCreacionista FeHist1y2 Rf25 NoCanartios NoMarzoAbr xGB
kaggle_modelo_6 <- read.table("modelo6_ganancias_log.txt", header = TRUE)[c(1:6),] # Linea de la muerte

# Une todos los modelos
kaggle_modelos <- rbind(kaggle_modelo_1, 
                        kaggle_modelo_2, 
                        kaggle_modelo_3,
                        kaggle_modelo_4 
                       # kaggle_modelo_5
                        )

kaggle_modelos$modelos <- c(rep("modelo_1",6), 
                            rep("modelo_2",6), 
                            rep("modelo_3",6),
                            rep("modelo_4",6) 
                            #rep("modelo_5",6)
                            )

# Me quedo con las variables que me importan: corte, ganancia y modelos
kaggle_modelos <- kaggle_modelos[,c(6,7,9)]

# Promedio de las ganancias de los modelos y los agrego a la tabla
promedio_ganancias <- kaggle_modelos %>%
  group_by(corte) %>%
  summarise(ganancia = mean(ganancia, na.rm = TRUE))
promedio_ganancias$modelos <- rep("promedio", nrow(promedio_ganancias))

kaggle_modelos <- rbind(kaggle_modelos, promedio_ganancias)

# Gráfico para comprar ganancias y elegir modelo y corte.
ggplot(kaggle_modelos, aes(x=corte, y=ganancia, colour = modelos))+
  geom_line()


# Generacion de archivo para kaggle según hibridación----

# Acá puse todos los modelos, pero tranquilamente se podrían soo agregar los de interés
modelo_1 <- fread("modelo1_tb_future_prediccion.txt", header = TRUE)
modelo_2 <- fread("modelo2_tb_future_prediccion.txt", header = TRUE)
modelo_3 <- fread("modelo3_tb_future_prediccion.txt", header = TRUE)
modelo_4 <- fread("modelo4_tb_future_prediccion.txt", header = TRUE)
#modelo_5 <- fread("modelo5_tb_future_prediccion.txt", header = TRUE)


# Combino los modelos para hacer un híbrido
# Cada individuo independiente debería elegir sus mejores modelos
# Todo es muy manual, con mas computer  literacy y tiempo seguro se puede automatizar un poco
modelos_promedio <- merge(modelo_1[,c(1,54)], modelo_2[,c(1,54)], by="numero_de_cliente")
colnames(modelos_promedio) <- c("numero_de_cliente",
                                "modelo_1",
                                "modelo_2")
modelos_promedio <- merge(modelos_promedio, modelo_3[,c(1,54)], by="numero_de_cliente")
colnames(modelos_promedio) <- c("numero_de_cliente",
                                "modelo_1",
                                "modelo_2",
                                "modelo_3")
modelos_promedio <- merge(modelos_promedio, modelo_4[,c(1,54)], by="numero_de_cliente")
colnames(modelos_promedio) <- c("numero_de_cliente",
                                "modelo_1",
                                "modelo_2",
                                "modelo_3",
                                "modelo_4")
#modelos_promedio <- merge(modelos_promedio, modelo_5[,c(1,54)], by="numero_de_cliente")
#colnames(modelos_promedio) <- c("numero_de_cliente",
#                                "modelo_1",
#                                "modelo_2",
#                                "modelo_3",
#                                "modelo_4",
#                                "modelo_5")


# Calculas el promedio para cada uno de los cortes
modelos_promedio$promedio <- rowMeans(modelos_promedio[, c("modelo_1",
                                                           "modelo_2",
                                                           "modelo_3", 
                                                           "modelo_4"
                                                           
                                                           #"modelo_5"
                                                           )
                                                       ], 
                                      na.rm = TRUE)

# Ordena las probabilidades según modelos_promedio
setorder(modelos_promedio, -promedio)



modelos_promedio$Predicted <- c(rep(1, 10500), rep(0, 165644-10500))
kaggle_hibrido_2_10500 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_2_10500,
          "kaggle_hibrido_2_10500.csv", row.names = FALSE)

modelos_promedio$Predicted <- c(rep(1, 11000), rep(0, 165644-11000))
kaggle_hibrido_2_11000 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_2_11000,
          "kaggle_hibrido_2_11000.csv", row.names = FALSE)


modelos_promedio$Predicted <- c(rep(1, 11500), rep(0, 165644-11500))
kaggle_hibrido_2_11500 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_2_11500,
          "kaggle_hibrido_2_11500.csv", row.names = FALSE)


modelos_promedio$Predicted <- c(rep(1, 12000), rep(0, 165644-12000))
kaggle_hibrido_2_12000 <- data.table(numero_de_cliente = modelos_promedio$numero_de_cliente,
                                   Predicted = modelos_promedio$Predicted)
write.csv(kaggle_hibrido_2_12000,
          "kaggle_hibrido_2_12000.csv", row.names = FALSE)



