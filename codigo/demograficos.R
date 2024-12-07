#######################     INSTALACIÓN DE PAQUETES     ######################
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#######################     DISTRIBUCIÓN POR GÉNERO     ######################

# Preparación de los datos 
datos_genero <- dataset %>%
  count(d_genero) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),  
    renombre_genero = recode(d_genero, "F" = "FEMENINO", "M" = "MASCULINO"),  
    etiqueta = paste0(n, " (", porcentaje, "%)") 
  )

# Creación del diagrama de torta
ggplot(datos_genero, aes(x = "", y = n, fill = renombre_genero)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  
  coord_polar("y") +                       
  geom_text(aes(label = etiqueta),          
            position = position_stack(vjust = 0.5), size = 4) + 
  labs(title = "Distribución por género", fill = "Género") +
  theme_void() +                           
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)) 

#######################     DISTRIBUCIÓN POR EDAD     ########################

# Cálculo del número de intervalos (usando la regla de Sturges)
n_datos <- nrow(dataset)  
rango <- max(dataset$d_edad) - min(dataset$d_edad)
intervalos <- ceiling(1 + log2(n_datos))  
amplitud <- floor(rango / intervalos)

min_edad <- floor(min(dataset$d_edad))  
max_edad <- ceiling(max(dataset$d_edad) + 2)  

rangos_edad <- cut(dataset$d_edad, 
                   breaks = seq(min_edad, max_edad, by = amplitud),  
                   include.lowest = TRUE, 
                   right = FALSE)  

# Agregación de la columna con los rangos en el dataset
dataset$rango_edad <- rangos_edad

# Creación del diagrama de barras para edad 
ggplot(dataset, aes(x = rango_edad)) +
  geom_bar(fill = "skyblue", color = "white") +  
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3) +  
  labs(title = "Distribución por edad", 
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
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "white") +  
  geom_text(aes(label = conteo), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 2.5) +  
  labs(title = "Distribución por edad y género", 
       x = "Rango de edad", y = "Frecuencia", fill = "Género") +  
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank()) 

# Creación del diagrama de bigotes para edad y sexo 
ggplot(dataset, aes(x = d_genero, y = d_edad, fill = d_genero)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  
  labs(title = "Distribución de edad por género",
       x = "Género", y = "Edad") +  
  theme(plot.title = element_text(hjust = 0.5),
                panel.background = element_blank())  +
  theme(legend.position = "none") 

########################     DISTRIBUCIÓN ÉTNICA     #########################

# Determinar si pertence o no a una etnia y agregar la columna al dataset
dataset$pertenece_etnia <- ifelse(dataset$etnia == "NINGUN GRUPO ETNICO", "NO PERTENECE", "PERTENECE")

# Creación del diagrama de torta que indica si pertenece o no a una etnia
ggplot(dataset, aes(x = "", fill = pertenece_etnia)) +
  geom_bar(stat = "count", width = 1, color = "white") +  
  coord_polar("y") +  
  geom_text(stat = "count", aes(label = paste0(..count.., " (", round(..count../sum(..count..)*100, 1), "%)")), 
            position = position_stack(vjust = 0.5), size = 4) +  
  labs(title = "Distribución étnica (pertenece o no a una etnia)", fill = "Etnia") +  
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5))  

# Filtrar datos para mostrar solo a los estudiantes que pertenecen a una etnia 
dataset_etnia <- dataset[dataset$pertenece_etnia == "PERTENECE", ]

# Creación del diagrama de barras para mostrar la distribución de los estudiantes que pertenecen a una etnia
ggplot(dataset_etnia, aes(y = etnia)) +
  geom_bar(fill = "purple", color = "white") +  
  geom_text(stat = "count", aes(label = ..count..), hjust = -0.2, size = 3) +  
  labs(title = "Distribución por grupo étnico", 
       x = "Frecuencia", y = "Grupo étnico") +  
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1))

####################     DISTRIBUCIÓN POR DISCAPACIDAD   #####################

# Determinar si tiene o no una discapacidad y agregar la columna al dataset
dataset$tiene_discapacidad <- ifelse(dataset$discapa == "NO TIENE DISCAPACIDAD", "NO TIENE", "TIENE")

# Creación del diagrama de torta que indica si tiene o no una discapacidad
ggplot(dataset, aes(x = "", fill = tiene_discapacidad)) +
  geom_bar(stat = "count", width = 1, color = "white") +  
  coord_polar("y") +  
  geom_text(stat = "count", aes(label = paste0(..count.., " (", round(..count../sum(..count..)*100, 1), "%)")), 
            position = position_stack(vjust = 0.5), size = 4) +  
  labs(title = "Distribución por discapacidad (tiene o no una discapacidad)", fill = "Discapacidad") +  
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Filtrar datos para mostrar solo a los estudiantes que tienen una discapacidad
dataset_discapacidad <- dataset[dataset$tiene_discapacidad == "TIENE", ]

# Creación del diagrama de barras para mostrar la distribución por discapacidad
ggplot(dataset_discapacidad, aes(x = discapa, fill = discapa)) +
  geom_bar(color = "white") +  
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +  
  scale_fill_brewer(palette = "Set1") +  
  labs(title = "Distribución por discapacidad", 
       x = "Discapacidad", y = "Frecuencia") +  
  theme(plot.title = element_text(hjust = 0.5),  
        panel.background = element_blank(),  
        axis.text.x = element_blank(),  
        legend.position = "right",  
        legend.title = element_blank())  

# Creación del diagrama de torta para mostrar la distribución por discapacidad

#################   DISTRIBUCIÓN POR TIPO DE DESPLAZAMIENTO  #################

# Definir una paleta de colores personalizada
ggplot(dataset, aes(y = d_tipo)) +
  geom_bar(fill = "red", color = "white") +  
  geom_text(stat = "count", aes(label = ..count..), hjust = -0.2, size = 3) +  
  labs(title = "Distribución por tipo de desplazamiento", 
       x = "Frecuencia", y = "Tipo de desplazamiento") +  
  theme(plot.title = element_text(hjust = 0.5),  
        panel.background = element_blank(),  
        axis.text.x = element_text(angle = 0, hjust = 1),  
        legend.position = "none")  

