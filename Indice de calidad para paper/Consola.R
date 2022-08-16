#Indice de calidad: .shp -> .shp, .csv , .png
#Toma el shape +Ciclo catastro base y lo procesa para devolver estadisticas basicas, indicadores de calidad
#, el shape inicial con calidad atribuida, y mapas


#Paquetes e instalación

#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
#                  "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "tmap"))
library(dplyr)
library(sf)
library(janitor)
library(ggplot2)
library("osmdata")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library("tmap")
library(extrafont)
library(ggspatial)
library(patchwork)
library(scico)
library(vapoRwave)

#Cargar funciones

#Data preparacion
source("Data prep.R")

#Data Calidad
source("Data calidad.R")
source("Data calidad Paper.R")
source("Data calidad objetivo.R")
source("Data calidad merkuria.R")
source("Data calidad decreto.R")

#Mapa Calidad
source("Mapa Calidad Merkuria.R")
source("Mapa Calidad Normativa.R")
source("Mapa Calidad Objetivo.R")


#Directorio
setwd("C:\\Users\\jajaj\\OneDrive\\Desktop\\Spatial Data\\Indicador de calidad ciclo\\Indicador de calidad para paper\\Indice de calidad para paper\\Funciones y Output\\")


# Limpieza de variables
#Merge.1 <- read.csv("C:\\Users\\jajaj\\OneDrive\\Desktop\\Spatial Data\\Indicador de calidad ciclo\\Indicador de calidad para paper\\Indice de calidad para paper\\Merge.1.csv")
df <- sf::st_read("C:\\Users\\jajaj\\OneDrive\\Desktop\\Spatial Data\\Indicador de calidad ciclo\\Indicador de calidad para paper\\Indice de calidad para paper\\Data raw\\Catastro_base.shp")

#===================================================================================================

# Descripción Funciones principales :

# 1.- GetDataReady: df -> df (Actualizado)
#     Funcion que busca un archivo tipo shp en la carpeta de data y lo lee. a su vez, tambien
#     genera un archivo csv con inconsistencias de la data y la envia a output. Estos archivos se deben revisar y limpiar
#     manualmente para luego ser agregados al archivo shp original y correr la funcion neuvamente hasta qu eno tenga errores.



# 2.- GetBasicAnalysis: GetDataReady(df) -> df (Actualizado), output/data basic/.csv, output/charts/.png
#     Funcion que se alimenta del resultado del archivo shp ya limpio a travez de GetShapeFile.
#     Esta funcion extrae tablas y graficos del analisis basico. los archivos salida van a la carpeta de outputs.


# 3.- GetQualityanalysis: GetDataReady(df) -> df (Actualizado),  output/data calidad/.csv, output/charts/.png
#     Funcion que a partir del archivo shp procesado con la funcion GetShapeFile, calcula un indice
#     de calidad y lo convierte en tablas que son escritas en formato csv y graficos en la carpeta output.

# 4.- MapaCalidad: output/data calidad/Index.csv -> output/charts/.png
#     Hace mapas simples de calidad.

#Comandos

# 1. - Preparacion data
        df <- GetDataReady(df)


# 2.- Estadisticas descriptivas
        GetBasicAnalysis(df)


# 3.- Analisis de Calidad

#   3.1 - Analisis calidad con la norma y mapa bajo la norma
          GetQualityAnalysisPaper(df)
#   3.2 - Analisis calidad no respetando la norma y mapa objetivo
          GetQualityAnalysisPObj(df)
#   3.3 - Analisis calidad merkuria y mapa merkuria
          GetQualityAnalysisMerkuria(df)
#   3.4 - Analisis calidad merkuria y mapa merkuria
          GetQualityDecreto(df)

# 4.- Mapas
#     4.1 - Calidad normativa
            MapaCalidadNormativa(df)
#     4.2 - Calidad normativa
            MapaCalidadObjetivo(df)
#     4.3 - Calidad normativa
            MapaCalidadMerkuria(df)

#
            index <- read.csv("output/data calidad merkuria/index.csv")
            df_m <- dplyr::bind_cols(df,index[,2:5])

            index <- read.csv("output/data calidad objetivo/index.csv")
            df_m <- dplyr::bind_cols(df_m,index[,2:5])

            index <- read.csv("output/data calidad normativa/index.csv")
            df_m <- dplyr::bind_cols(df_m,index[,2:5])

write_sf(df_m,"output/df_m", driver="ESRI Shapefile")
