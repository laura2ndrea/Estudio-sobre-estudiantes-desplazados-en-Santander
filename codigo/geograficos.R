# Instalar y cargar librerías necesarias
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(sf)) install.packages("sf")

library(dplyr)       # Para manipulación de datos
library(ggplot2)     # Para visualización
library(sf)          # Para trabajar con datos espaciales


# Paso 1: Procesamiento de datos
# Convertir los datos a un marco de datos para su manipulación
data <- as.data.frame(dataset)

# Verificar columnas disponibles
print(names(data))

# Adaptar según las columnas específicas del dataset
data <- data %>% 
  select(d_nombmuni, d_nomzon, d_hombres, d_mujeres) %>% 
  rename(municipio = d_nombmuni, principio = d_nomzon, hombres = d_hombres, mujeres = d_mujeres) %>% 
  mutate(hombres = as.numeric(hombres),
         mujeres = as.numeric(mujeres),
         poblacion = hombres + mujeres,
         municipio = toupper(trimws(municipio))) %>% 
  group_by(municipio, principio) %>% 
  summarise(total_poblacion = sum(poblacion, na.rm = TRUE))

# Paso 2: Leer datos espaciales (shapefile)
shape_data <- st_read("MGN_ADM_MPIO_GRAFICO.shp")

# Verificar nombres de columnas en el shapefile
print(names(shape_data))

# Mostrar las primeras filas del shapefile para inspeccionar los datos
head(shape_data)

# Filtrar solo los municipios del departamento de Santander
shape_data <- shape_data %>% 
  filter(dpto_cnmbr == "SANTANDER") %>%  # Filtrar por departamento
  mutate(mpio_cnmbr = toupper(trimws(mpio_cnmbr)))  # Normalizar nombres de municipios

# Unir los datos del shapefile con los datos de población
shape_data <- shape_data %>% 
  left_join(data, by = c("mpio_cnmbr" = "municipio"))

# Verificar si la unión se realizó correctamente
if (!"total_poblacion" %in% names(shape_data)) {
  stop("No se pudo unir correctamente los datos de población con los datos espaciales. Revisa los nombres de las columnas.")
}

# Paso 3: Crear el mapa de calor con nombres de los municipios
ggplot(shape_data) +
  geom_sf(aes(fill = total_poblacion), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey") +  # Áreas sin datos en blanco
  geom_sf_text(
    data = shape_data %>% filter(!is.na(total_poblacion)),  # Solo municipios con datos de población
    aes(label = mpio_cnmbr),
    size = 3, color = "black", check_overlap = TRUE  # Tamaño y color del texto
  ) +
  labs(title = "Distribución de la población desplazada por municipios en Santander",
       fill = "Población") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
