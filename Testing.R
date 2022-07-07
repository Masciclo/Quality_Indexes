# Autor: Ruben Dario Lopez.
# E-mail: macv1031@gmail.com.
# Github: macv1031
# Fecha: enero, 2021
#
# Descripcion: Este es el script de testing para correr las funciones creadas en el script main.R


# Reading function to get data set ----
data_shape <- GetShapeFile(file = "ciclovias_unidas")


# Reading function to get index ----
GetBasicAnalysis(df = data_shape)


# Reading function to get index ----
GetQualityAnalysis(df = data_shape)
