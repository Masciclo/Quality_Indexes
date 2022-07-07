setwd("C:/Users/...")

library(sf)
library(sp)
library(maptools)

# import the data
shp.file <-  st_read("data/Catastro_2021.shp")

ncols <-  39 #N of simple features

#Check the Geometry type
class(st_geometry(st_read("data/Catastro_2021.shp", quiet = TRUE)))

#LINESTRINGs
shp_geom <- st_geometry(shp.file)
shp_geom <- shp_geom[!st_is_empty(shp_geom)]

# create objects of class SpatialLine
point1 <- st_coordinates(shp_geom[1])[,1:2]
line1 <- list(Line(point1))
sp_lines <- SpatialLines(list(Lines(line1, ID = "line1")))


for (i in 2:length(shp_geom)) {
  id <- paste0("line", as.character(i))
  l <- SpatialLines(list(Lines(list(Line(st_coordinates(shp_geom[i])[,1:2] )),id)))
  sp_lines <- spRbind(sp_lines, l)
}

# DataFrame
df = as.data.frame(shp.file)
df = df[!st_is_empty(df$geometry),]

#SpatialLines to SpatialLinesDataFrame
SLdf <- SpatialLinesDataFrame(sp_lines, data = df[,1:ncols], match.ID = FALSE)

#graphic validation
#spplot(SLdf)


