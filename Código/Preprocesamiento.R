###################     INSTALACIÓN DE PAQUETES     ##########################
install.packages("skimr")
install.packages("dplyr")
library(dplyr)
library(skimr)

###################     CARGADO DEL DATASET     ##############################

dataset <- read.csv("Estudiantes_en_situacion_de_desplazamiento_Santander.csv")

###################     EXPLORACIÓN DEL DATASET     ##########################

# Dimensiones del dataset
dim(dataset)

# Estructura del dataset
str(dataset)

# Resumen estadístico general de dataset 
summary(dataset)
skim(dataset)

# Revisión de los primeros y últimos registros 
head(dataset, 10) 
tail(dataset, 10)

# Búsqueda de valores faltantes 
sum(is.na(dataset))

# Exploración de columnas de interes 

## Resumen estadistico 
summary(dataset$d_grado)
summary(dataset$d_edad)

## Valores únicos 
unique(dataset$d_nombmuni)
unique(dataset$d_provincia)
unique(dataset$d_genero)
unique(dataset$d_ano)
unique(dataset$d_tipo)
unique(dataset$metodo)
unique(dataset$etnia)
unique(dataset$discapa)
unique(dataset$d_nomsec)
unique(dataset$d_nomzon)
unique(dataset$d_grado)

###################     LIMPIEZA DEL DATASET     #########################

# Eliminación de columnas inncesarias para nuestro analisis

## Verificación de que edad y d_edad contienen la misma información 
identical(dataset$edad, dataset$d_edad)

dataset <- dataset %>%
  select(-edad, -d_nomjor, -d_hombres, -d_mujeres, -sector)

# Conversión de la columna dane_ant a 'character'
dataset$dane_ant <- as.character(dataset$dane_ant)

# Verificación del dataset luego de la limpieza
dim(dataset)
str(dataset)
