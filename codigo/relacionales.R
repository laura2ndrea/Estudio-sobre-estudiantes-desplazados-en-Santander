#######################     INSTALACIÓN DE PAQUETES     ######################

if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(tidyr)) install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(tidyr)

###############     RELACIÓN ENTRE MÉTODO EDUCATIVO Y SEXO     ##############

# Selección de las columnas necesarias y renombrarlas
genero_data <- dataset %>%
  select(metodo, d_hombres, d_mujeres) %>%  
  rename(metodo = metodo, hombres = d_hombres, mujeres = d_mujeres) %>%  
  mutate(hombres = as.numeric(hombres),  
         mujeres = as.numeric(mujeres)) %>% 
  group_by(metodo) %>%  
  summarise(total_hombres = sum(hombres, na.rm = TRUE),  
            total_mujeres = sum(mujeres, na.rm = TRUE)) %>%  
  pivot_longer(cols = c(total_hombres, total_mujeres),  
               names_to = "genero",  
               values_to = "cantidad")  

# Crear el gráfico de dispersión
ggplot(genero_data, aes(x = metodo, y = cantidad, color = genero)) +
  geom_point(size = 5, alpha = 0.7) +  
  scale_y_continuous(limits = c(0, max(genero_data$cantidad))) +  
  labs(title = "Relación entre método educativo y sexo",  
       x = "Método educativo",  
       y = "Cantidad",  
       color = "Sexo") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5))  

##########     RELACIÓN ENTRE MÉTODO EDUCATIVO Y GRUPO ÉTNICO     ###########

# Selección las columnas necesarias y renombrarlas
etnico_data <- dataset %>%
  select(metodo, etnia, d_hombres, d_mujeres) %>% 
  rename(metodo = metodo, grupo_etnico = etnia, hombres = d_hombres, mujeres = d_mujeres) %>%  
  mutate(hombres = as.numeric(hombres),  
         mujeres = as.numeric(mujeres),  
         grupo_etnico = toupper(trimws(grupo_etnico))) %>% 
  group_by(metodo, grupo_etnico) %>%  
  summarise(total_poblacion = sum(hombres + mujeres, na.rm = TRUE))  

# Creación del gráfico de dispersión
ggplot(etnico_data, aes(x = metodo, y = total_poblacion, color = grupo_etnico)) +
  geom_point(size = 5, alpha = 0.7) +  
  labs(title = "Relación entre método educativo y grupo étnico",  
       x = "Método educativo",  
       y = "Población total", 
       color = "Grupo étnico") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5))  # Centrar el título del gráfico

###############     RELACIÓN ENTRE MÉTODO EDUCATIVO Y EDAD     ##############

# Selección de las columnas necesarias 
edad_data <- dataset %>%
  select(metodo, d_edad) %>%  
  rename(metodo = metodo, edad = d_edad) %>%  
  mutate(edad = as.numeric(edad)) %>% 
  group_by(metodo) %>%  
  summarise(promedio_edad = mean(edad, na.rm = TRUE))  

# Creación del gráfico de dispersión
ggplot(edad_data, aes(x = metodo, y = promedio_edad)) +
  geom_point(color = "purple", size = 5, alpha = 0.7) +  
  labs(title = "Relación entre método educativo y promedio de edad",  
       x = "Método educativo",  
       y = "Promedio de edad") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5))  

######    RELACIÓN ENTRE MOTIVO DE DESPLAZAMIENTO Y GRUPO ÉTNICO     #######

# Convertir el conjunto de datos a un data frame
data <- as.data.frame(dataset)  

# Procesar y transformar los datos
displacement_data <- data %>%
  select(d_tipo, etnia, d_hombres, d_mujeres) %>%  
  rename(motivo = d_tipo, grupo_etnico = etnia, hombres = d_hombres, mujeres = d_mujeres) %>%  
  mutate(hombres = as.numeric(hombres), 
         mujeres = as.numeric(mujeres),
         grupo_etnico = toupper(trimws(grupo_etnico)),  
         motivo = toupper(trimws(motivo))) %>%  
  group_by(motivo, grupo_etnico) %>% 
  summarise(total_poblacion = sum(hombres + mujeres, na.rm = TRUE))  

# Crear el gráfico de dispersión
ggplot(displacement_data, aes(x = motivo, y = grupo_etnico, size = total_poblacion, color = grupo_etnico)) +
  geom_point(alpha = 0.7) +  
  labs(title = "Relación entre motivo de desplazamiento y grupo étnico",  
       x = "Motivo de desplazamiento",  
       y = "Grupo étnico",  
       size = "Población total",  
       color = "Grupo étnico") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5))  
