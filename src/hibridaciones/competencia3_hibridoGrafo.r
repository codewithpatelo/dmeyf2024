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
corte <- c(10500, 11000, 11500, 12000)
ganancia_hibrido_1 <- c(130.817, 132.567, 129.207, 134.177)
modelito_1 <- rep("hibrido_1", length(corte))
kaggle_hibrido_1 <- data.frame("corte" = corte, "ganancia" = ganancia_hibrido_1, "modelos" = modelito_1)

ganancia_hibrido_2 <- c(131.657,  127.037, 131.797, 133.757)
modelito_2 <- rep("hibrido_2", length(corte))
kaggle_hibrido_2 <- data.frame("corte" = corte, "ganancia" = ganancia_hibrido_2, "modelos" = modelito_2)

kaggle_modelos <- rbind(kaggle_modelos, kaggle_hibrido_1, kaggle_hibrido_2)

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
