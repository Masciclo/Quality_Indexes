#Toda la red
#osm_prueba <- sf::st_read("C:\\Users\\jajaj\\OneDrive\\Desktop\\Spatial Data\\Mazo de cartas\\Osm calles\\Osm_calles.shp")
#Muestra
osm_prueba <- sf::st_read("C:\\Users\\jajaj\\OneDrive\\Desktop\\Spatial Data\\Proyectos\\Avenida Matta\\Red open street\\Red_open_street.shp")

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("osmdata")
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")

bbx <- getbb("Boston, MA")

#Bounding box
min_lon <-  -70.80; max_lon <- -70.50
min_lat <- -33.60; max_lat <- -33.30
bbx <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
colnames(bbx) <- c("min","max")

#1) Mapa de tipologia
# ==================================
#Data rdy
color_calles <- rgb(0.42,0.449,0.488)
low_stress <- dplyr::filter(osm_prueba, type %in% c("residential","living_street"))
medium_stress <- dplyr::filter(osm_prueba, type %in% c( "tertiary" , "tertiary_link" ))
high_stress <- dplyr::filter(osm_prueba, type %in% c("primary" ,  "secondary" , "primary_link" ))

#Mapa
ggplot(data = df ) +
  #Open street map

  #geom_sf(data = osm_prueba, aes(color = type), size = .4,
  #         alpha = 0.65) +
  geom_sf(data = low_stress, col= color_calles, size = .3,
          alpha = 0.6) +
  geom_sf(data = high_stress, col = color_calles, size = .6,
          alpha = 0.8) +
  #ciclovias
  geom_sf(aes(color = tipci)) +
          scale_color_brewer(palette = "Spectral") +

  #Barra de escala
  annotation_scale(location = "bl", width_hint = 0.4, line_width = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  #Titulo
  ggtitle("Santiago", subtitle = paste0("(Tipologias)")) +

  #Bounding box
  coord_sf(xlim = c(as.integer(min_lon),as.integer(max_lon)),
           ylim = c(as.integer(min_lat),as.integer(max_lat)),
           expand = FALSE) +
  #tema
    theme_void()

#2) Mapa Calidad

# Data rdy
index <- read.csv("output/data/index.csv")
df <- dplyr::bind_cols(df,index[,2:5])
low_stress <- dplyr::filter(osm_prueba, type %in% c("residential","living_street"))
medium_stress <- dplyr::filter(osm_prueba, type %in% c( "tertiary" , "tertiary_link" ))
high_stress <- dplyr::filter(osm_prueba, type %in% c("primary" ,  "secondary" , "primary_link" ))


op_ci_0 <- dplyr::filter(df, op_ci == 0)
op_ci_1 <- dplyr::filter(df, op_ci == 1)

#Color osm
color_calles <- rgb(0.42,0.449,0.488)

#Mapa
    ggplot() +
  #Osm
     # geom_sf(data = low_stress, col= color_calles, size = .2,
      #        alpha = 0.8) +
      #geom_sf(data = medium_stress, col= color_calles, size = .3,
      #        alpha = 0.8) +
      #geom_sf(data = high_stress, col = color_calles, size = .4,
      #        alpha = 0.9) +
  #ciclovias
      geom_sf(data = op_ci_0, col= "#E04F58", size = .6,
              alpha = 0.6) +
      geom_sf(data = op_ci_1, col= "#13D657", size = .6,
              alpha = 0.6) +
      theme_void()





