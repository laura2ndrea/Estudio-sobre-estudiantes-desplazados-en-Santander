#######################     INSTALACIÓN DE PAQUETES     ######################
install.packages("ggplot2")
install.packages("dplyr")
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

#######################     DISTRIBUCIÓN POR METODO     ######################

# Preparación de los datos 
datos_metodo <- dataset %>%
  count(metodo) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2),
    etiqueta = paste0(n, " (", porcentaje, "%)")  
  )

# Creación del diagrama de torta
ggplot(datos_metodo, aes(x = "", y = n, fill = metodo)) +
  geom_bar(stat = "identity", width = 1) +  
  coord_polar("y") +                       
  geom_text(aes(label = etiqueta),          
            position = position_stack(vjust = 0.5), size = 4) + 
  labs(title = "Distribución por metodo", fill = "Metodo") +
  theme_void() +                           
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)) 

#####################     DISTRIBUCIÓN POR INSTITUTO     #####################

# Agrupar por institución y contar el número de estudiantes en cada una
conteo_inst <- dataset %>%
  group_by(d_nombinst) %>%
  summarise(total_estudiantes = n()) %>%  
  arrange(desc(total_estudiantes))  # 

# Filtrar las instituciones con más estudiantes desplazados
est_x_inst <- head(conteo_inst, 10)

# Creación del diagrama de barras para instituciones con mas estudiantes 
ggplot(est_x_inst, aes(x = reorder(d_nombinst, -total_estudiantes), y = total_estudiantes)) +
  geom_bar(stat = "identity", fill = "violet") + 
  geom_text(aes(label = total_estudiantes), vjust = -0.5) +
  labs(title = "Instituciones con más estudiantes desplazados",
       x = "Institución", 
       y = "Número de estudiantes desplazados") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####################     DISTRIBUCIÓN POR SEDES     #########################

# Contar cuántas sedes ha absorbido cada institución
inst_sedes <- dataset %>%
  group_by(d_nombinst) %>%
  summarise(sedes_absorbidas = n_distinct(d_sede)) %>%
  arrange(desc(sedes_absorbidas))

# Ver las 10 instituciones con más sedes absorbidas
inst_x_sedes <- head(inst_sedes, 10)

ggplot(inst_x_sedes, aes(x = reorder(d_nombinst, -sedes_absorbidas), y = sedes_absorbidas)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sedes_absorbidas), vjust = -0.5) +
  labs(title = "Instituciones que más colegios pequeños han absorbido como sedes",
       x = "Institución", 
       y = "Número de sedes absorbidas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

