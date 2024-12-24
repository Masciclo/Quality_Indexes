![Ambito](https://github.com/Masciclo/Indice-calidad/blob/main/Portada%20Catastro%20PNG.png)

## Descripción

![Noticia](https://github.com/user-attachments/assets/8cd390a9-727f-46a2-9fe8-5be8a7586db3)
![Mapa reprobadas](https://github.com/user-attachments/assets/6c0972b6-ba99-41ce-a144-5df0ab5b16de)
![Tipos de reprobaciones](https://github.com/user-attachments/assets/a2b1cb82-87be-4844-8ecf-bea4096f159b)

## Estado del Proyecto

Desde una archivo shapefile con las variables mostrada en la fig.1 se limpia la data inconsistente del catastro manualmente hecho con la aplicación mapmaker. Se sacan algunas tablas de resumen por tipologia.
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

![*fig.1* Variables del catastro](https://github.com/Masciclo/Indice-calidad/blob/main/Ambito%20de%20catastro.png)
![Variables PNG](https://github.com/user-attachments/assets/415f8426-4924-41fa-b599-52d0903519d9)

## Instalación

