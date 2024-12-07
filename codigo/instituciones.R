#######################     INSTALACIÓN DE PAQUETES     ######################
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
library(ggplot2)
library(dplyr)

###############     DISTRIBUCIÓN POR SECTOR DE LA INSTITUCION     ############

# Preparación de los datos 
datos_nomsec <- dataset %>%
  count(d_nomsec) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),  
    renombre_nomsec = recode(d_nomsec, "O" = "Oficial", "P" = "Privado"),  
    etiqueta = paste0(n, " (", porcentaje, "%)") 
  )

# Creación del diagrama de torta
ggplot(datos_nomsec, aes(x = "", y = n, fill = renombre_nomsec)) +
  geom_bar(stat = "identity", width = 1) +  
  coord_polar("y") +                       
  geom_text(aes(label = etiqueta),          
            position = position_stack(vjust = 0.5), size = 4) + 
  labs(title = "Distribución por sector de la institucion", fill = "Sector") +
  theme_void() +                           
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)) 

###############     DISTRIBUCIÓN POR ZONA DE LA INSTITUCION     ############

# Preparación de los datos
zona_data <- dataset %>%
  select(d_nomzon) %>%  # Seleccionar la columna correspondiente
  rename(zona = d_nomzon) %>%  # Renombrar para mayor claridad
  mutate(zona = toupper(trimws(zona))) %>%  # Normalizar los nombres
  group_by(zona) %>%  # Agrupar por zona
  summarise(cantidad = n()) %>%  # Contar la cantidad de instituciones por zona
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 2),  # Calcular el porcentaje
         etiqueta = paste0(cantidad, " (", porcentaje, "%)"))  # Crear la etiqueta

# Verificar los datos agrupados
print(zona_data)

# Crear el diagrama de torta
ggplot(zona_data, aes(x = "", y = cantidad, fill = zona)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 4) +  # Agregar las etiquetas
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribución de instituciones por zona (rural/urbana)",
       fill = "Zona") +
  theme_void() +
  theme(axis.text.x = element_blank(),  
        axis.ticks = element_blank())  

#######################     DISTRIBUCIÓN POR METODO     ######################

# Preparación de los datos 
datos_metodo <- dataset %>%
  count(metodo) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2),
    etiqueta = paste0(n, " (", porcentaje, "%)")  
  )

# Creación del diagrama de torta
ggplot(datos_metodo, aes(x = metodo, y = porcentaje, fill = metodo)) +
  geom_bar(stat = "identity", fill = "orange", width = 0.7) +  
  geom_text(aes(label = etiqueta), vjust = 0.5, color = "black", size = 3, hjust = 0.3) + 
  labs(title = "Distribución por método", 
       x = "Método",
       y = "Porcentaje de distribución") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

#####################     DISTRIBUCIÓN POR INSTITUTO     #####################

# Contar el número de estudiantes por institución y ordenar de mayor a menor
conteo_inst <- dataset %>%
  group_by(d_nombinst) %>%  # Agrupar por el nombre de la institución
  summarise(total_estudiantes = n()) %>%  # Calcular el total de estudiantes en cada institución
  arrange(desc(total_estudiantes))  # Ordenar las instituciones de mayor a menor cantidad de estudiantes

# Seleccionar las primeras 10 instituciones con más estudiantes
est_x_inst <- head(conteo_inst, 10)

# Creación del gráfico de barras con las 10 instituciones con mas estudiantes
ggplot(est_x_inst, aes(x = reorder(d_nombinst, total_estudiantes), y = total_estudiantes, fill = d_nombinst)) +
  geom_bar(stat = "identity", color = "white") +  
  geom_text(aes(label = total_estudiantes), vjust = -0.5, size = 2.5) +  
  scale_fill_brewer(palette = "Set3") +  
  labs(title = "Instituciones con más estudiantes desplazados",  
       x = "Institución", y = "Estudiantes desplazados") +  
  theme(plot.title = element_text(hjust = -1),  
        panel.background = element_blank(),  
        axis.text.x = element_blank(),  
        legend.position = "right",  
        legend.title = element_blank())  

