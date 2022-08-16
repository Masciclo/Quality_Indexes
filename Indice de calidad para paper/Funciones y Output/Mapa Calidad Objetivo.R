MapaCalidadObjetivo <- function(df) {
  #Mapa calidad df, osm -> chart calidad
  #Esta funcion recibe y filtra por calidad data frame ciclo y lee osm para transformarlos en un mapa.


  osm_prueba <- sf::st_read("C:\\Users\\jajaj\\OneDrive\\Desktop\\Spatial Data\\Indicador de calidad ciclo\\Indicador de calidad para paper\\ejes open street map\\ejes_open_street_map.shp")


  #2) Mapa Calidad

  # Data rdy
  index <- read.csv("output/data calidad objetivo/index.csv")
  df_m <- dplyr::bind_cols(df,index[,2:5])
  low_stress <- dplyr::filter(osm_prueba, type %in% c("residential","living_street"))
  medium_stress <- dplyr::filter(osm_prueba, type %in% c( "tertiary" , "tertiary_link" ))
  high_stress <- dplyr::filter(osm_prueba, type %in% c("primary" ,  "secondary" , "primary_link" ))


  op_ci_0 <- dplyr::filter(df_m, op_ci == 0)
  op_ci_1 <- dplyr::filter(df_m, op_ci == 1)

  mts_totales <- sum(df_m$largo_m)
  Reprobadas <- round(sum(op_ci_1$largo_m)/mts_totales, 2)*100
  Aprobadas <- 100 - Reprobadas

  #Color osm
  color_calles <- rgb(0.42,0.449,0.488)

  #Limites mapa
  min_lon <-  -70.80; max_lon <- -70.50
  min_lat <- -33.65; max_lat <- -33.30
  bbx <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))

  #Mapa
  mapacalidadobj <- ggplot() +
    #Osm
    #geom_sf(data = low_stress, color = alpha(color_calles,0.2), size = .2) +
    #geom_sf(data = medium_stress, color = alpha(color_calles,0.2), size = .3) +
    #geom_sf(data = high_stress, col = alpha(color_calles,0.2), size = .4) +
    #ciclovias
    geom_sf(data = op_ci_0, col= "#13D657", size = .6,
            alpha = 0.6) +
    guides(col = guide_legend(title = NULL, byrow = T )) +
    geom_sf(data = op_ci_1, col= "#D6648E", size = .6,
            alpha = 0.6) +
    guides(col = guide_legend(title = NULL, byrow = T )) +
    #Barra de escala
    annotation_scale(location = "bl", width_hint = 0.4, line_width = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering)+
    #Titulo
    ggtitle("Santiago", subtitle = paste("(Calidad propuesta: ","Reprobadas = ", Reprobadas,"%, Aprobadas = ", Aprobadas, "%)")) +

    #Cordenadas
    coord_sf(crs = st_crs(4326), xlim = bbx[1,], ylim = bbx[2,],
      expand = FALSE,label_axes = element_blank(), datum = NA) +
    theme_bw()
  ggsave("output/mapa calidad objetivo/Mapa_calidad.png"
         ,width=10, height=8,dpi=300)
  return(mapacalidadobj)


}


