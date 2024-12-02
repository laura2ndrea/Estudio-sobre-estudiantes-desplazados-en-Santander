###################     INSTALACIÓN DE PAQUETES     ##########################
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

###################     DISTRIBUCIÓN POR GÉNERO     ##########################

# Preparación de los datos 
datos_genero <- dataset %>%
  count(d_genero) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),  
    renombre_genero = recode(d_genero, "F" = "Femenino", "M" = "Masculino"),  
    etiqueta = paste0(n, " (", porcentaje, "%)") 
  )

# Creación del diagrama de torta
ggplot(datos_genero, aes(x = "", y = n, fill = renombre_genero)) +
  geom_bar(stat = "identity", width = 1) +  
  coord_polar("y") +                       
  geom_text(aes(label = etiqueta),          
            position = position_stack(vjust = 0.5), size = 4) + 
  labs(title = "Distribución por género", fill = "Género") +
  theme_void() +                           
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)) 

###################     DISTRIBUCIÓN POR EDAD     ############################

# Cálculo del número de intervalos (usando la regla de Sturges)
n_datos <- nrow(dataset)  
rango <- max(dataset$d_edad) - min(dataset$d_edad)
intervalos <- ceiling(1 + log2(n_datos))  
amplitud <- floor(rango / intervalos)

min_edad <- floor(min(dataset$edad))  
max_edad <- ceiling(max(dataset$edad) + 2)  

# Creación de los rangos de edad 
rangos_edad <- cut(dataset$d_edad, 
                   breaks = seq(min_edad, max_edad, by = amplitud),  
                   include.lowest = TRUE, 
                   right = FALSE)  

# Agregación de la columna con los rangos en el dataset
dataset$rango_edad <- rangos_edad

# Creación del diagrama de barras para edad 
ggplot(dataset, aes(x = rango_edad)) +
  geom_bar(fill = "skyblue", color = "black") +  
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +  
  labs(title = "Distribución por edad (rangos)", 
       x = "Rangos de edad", y = "Frecuencia") +  
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())

###################     DISTRIBUCIÓN POR EDAD Y GÉNERO     ###################

# Agrupación y resumen de los datos 
datos_barras <- dataset %>%
  group_by(rango_edad, d_genero) %>%
  summarise(conteo = n(), .groups = "drop")  

# Creación del diagrama de barras para edad y sexo 
ggplot(datos_barras, aes(x = rango_edad, y = conteo, fill = d_genero)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +  # Barras agrupadas
  geom_text(aes(label = conteo), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 2.5) +  
  labs(title = "Distribución de edad y género", 
       x = "Rango de edad", y = "Frecuencia", fill = "Género") +  
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank()) 

# Creación del diagrama de bigotes para edad y sexo 
ggplot(dataset, aes(x = d_genero, y = d_edad, fill = d_genero)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Diagrama de bigotes
  labs(title = "Distribución de edad por género",
       x = "Género", y = "Edad") +  
  theme(plot.title = element_text(hjust = 0.5),
                panel.background = element_blank())  +
  theme(legend.position = "none")  

# Distribución étnica (diagrama de barras)

# Distribución por discapacidad (diagrama de torta)

# Distribución por tipo de desplazamiento (diagrama de barras)