###################     INSTALACIÓN DE PAQUETES     ##########################
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

###################     ANÁLISIS DEMOGRÁFICO     ##########################

# Distribución por género (diagrama de torta)

## Preparación de los datos 
datos_genero <- dataset %>%
  count(d_genero) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),  
    renombre_genero = recode(d_genero, "F" = "Femenino", "M" = "Masculino"),  
    etiqueta = paste0(n, " (", porcentaje, "%)") 
  )

## Creación del diagrama de torta 
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

# Distribución por edad (diagrama de barras) 

## Calculo del número de intervalos utilizando la regla de Sturges
n_datos <- nrow(dataset)  
rango <- max(dataset$d_edad) - min(dataset$d_edad)
intervalos <- ceiling(1 + log2(n_datos))  
amplitud <- floor(rango / intervalos)

min_edad <- floor(min(dataset$edad))  
max_edad <- ceiling(max(dataset$edad) + 2)  

## Creación de los rangos de edad 
rangos_edad <- cut(dataset$d_edad, 
                   breaks = seq(min_edad, max_edad, by = amplitud),  
                   include.lowest = TRUE, 
                   right = FALSE)  

## Agregación de la columnas con los rangos al dataset
dataset$rango_edad <- rangos_edad

## Creación del diagrama de barras 
ggplot(dataset, aes(x = rango_edad)) +
  geom_bar(fill = "skyblue", color = "black") +  
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +  
  labs(title = "Distribución por edad (rangos)", 
       x = "Rangos de edad", y = "Frecuencia") +  
  theme(plot.title = element_text(hjust = 0.5)) 

# Distribución por edad (diagrama de bigotes)

# Distribución étnica (diagrama de barras)

# Distribución por discapacidad (diagrama de torta)

# Distribución por tipo de desplazamiento (diagrama de barras)