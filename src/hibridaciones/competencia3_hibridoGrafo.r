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
modelo_5 <- fread("modelo5_tb_future_prediccion.txt", header = TRUE)


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
modelos_promedio <- merge(modelos_promedio, modelo_5[,c(1,54)], by="numero_de_cliente")
colnames(modelos_promedio) <- c("numero_de_cliente",
                                "modelo_1",
                                "modelo_2",
                                "modelo_3",
                                "modelo_4",
                                "modelo_5")


# Calculas el promedio para cada uno de los cortes
modelos_promedio$promedio <- rowMeans(modelos_promedio[, c("modelo_1",
                                                           "modelo_2",
                                                           "modelo_3", 
                                                           "modelo_4",
                                                           
                                                           "modelo_5"
)
], 
na.rm = TRUE)

# Ordena las probabilidades según modelos_promedio
setorder(modelos_promedio, -promedio)

# Filtra los datos para mostrar solo entre cortes 10500 y 12000
kaggle_modelos_filtrado <- kaggle_modelos %>% 
  filter(corte >= 10500 & corte <= 12000)

# Crear el gráfico con los datos filtrados
ggplot(kaggle_modelos_filtrado, aes(x = corte, y = ganancia, colour = ganancia, group = modelos)) +
  geom_line() +  # Crear líneas
  scale_colour_gradient(low = "red", high = "green") +  # Rojo para menor ganancia, verde para mayor ganancia
  geom_text(aes(label = modelos), hjust = -0.1, vjust = -0.5, size = 3, show.legend = FALSE) +  # Etiquetas para cada modelo
  geom_hline(yintercept = 135.296, linetype = "dashed", color = "darkgrey") +  # Línea horizontal en y = 135.296
  annotate("text", x = max(kaggle_modelos_filtrado$corte) + 100, y = 135.296, 
           label = "Línea de la Muerte", color = "darkgrey", size = 4) +  # Etiqueta para la línea
  # Agregar el punto gris oscuro y la etiqueta "modelo_6"
  geom_point(aes(x = 11000, y = 130.047), color = "darkgrey", size = 3) + 
  annotate("text", x = 11000, y = 130.047, label = "modelo_6", color = "darkgrey", vjust = -1.5, size = 4) +
  # Línea verde punteada para "Votación 2"
  geom_hline(yintercept = 136.486, linetype = "dotted", color = "green") +  # Línea verde punteada
  annotate("text", x = max(kaggle_modelos_filtrado$corte) + 100, y = 136.486, 
           label = "Votación 2", color = "green", size = 4) +  # Etiqueta para la línea
  theme(
    panel.background = element_rect(fill = "grey90"),  # Fondo gris claro
    plot.background = element_rect(fill = "grey90"),  # Fondo gris claro para todo el gráfico
    panel.grid.major = element_line(color = "white", size = 0.5),  # Líneas de la cuadrícula mayores en blanco
    panel.grid.minor = element_line(color = "white", size = 0.25),  # Líneas de la cuadrícula menores en blanco
    text = element_text(color = "black")  # Texto en negro
  ) +
  labs(title = "Comparación de Ganancias por Modelo (Cortes 10500-12000)", 
       x = "Corte", 
       y = "Ganancia")