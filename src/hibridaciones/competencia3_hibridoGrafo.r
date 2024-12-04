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
