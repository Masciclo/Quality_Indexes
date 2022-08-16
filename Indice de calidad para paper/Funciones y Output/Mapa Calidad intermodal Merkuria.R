#Data

  #Transantiago
    tstg.df <- sf::st_read("C:\\Users\\jajaj\\OneDrive\\Desktop\\Spatial Data\\Mazo de cartas\\Recorridos_Transantiago-shp\\65f69d7c-a0ee-4ff5-b220-0168a3c2b756202041-1-16ubwo1.mcsv.shp")
    #Color osm
    color_tstg <- rgb(100/255,77/255,190/255)

  #OSM
    osm_prueba <- sf::st_read("C:\\Users\\jajaj\\OneDrive\\Desktop\\Spatial Data\\Proyectos\\Avenida Matta\\Red open street\\Red_open_street.shp")

    low_stress <- dplyr::filter(osm_prueba, type %in% c("residential","living_street"))
    medium_stress <- dplyr::filter(osm_prueba, type %in% c( "tertiary" , "tertiary_link" ))
    high_stress <- dplyr::filter(osm_prueba, type %in% c("primary" ,  "secondary" , "primary_link" ))
    #Color osm
    color_calles_lts_1 <- rgb(153/255,193/255,64/255)
    color_calles_lts_2 <- rgb(219/255,123/255,43/255)
    color_calles_lts_3 <- rgb(204/255,20/255,20/255)

  #Ciclo high
    index <- read.csv("output/data calidad objetivo/index.csv")
    df_m <- dplyr::bind_cols(df,index[,2:5])
    op_ci_0 <- dplyr::filter(df_m, op_ci == 0)
    op_ci_1 <- dplyr::filter(df_m, op_ci == 1)
    color_op_ci_0 <- rgb(41/255,201/255,55/255)
  #Comunas
    comunas <- sf::st_read("C:\\Users\\jajaj\\OneDrive\\Desktop\\Spatial Data\\Mazo de cartas\\comunas  gran stg\\comunas_gran_stg.shp")
    color_comuna <- rgb(35/255,35/255,35/255)
  #Limites mapa
    min_lon <-  -70.80; max_lon <- -70.50
    min_lat <- -33.65; max_lat <- -33.30
    bbx <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))

#Mapa
  mapa.lts.Merkuria <- ggplot() +
    #Comunas
    geom_sf(data = comunas, color = alpha(color_comuna,0.2), size = .2) +
    #Ciclo
    geom_sf(data = op_ci_0, color = alpha(color_op_ci_0,0.9), size = .2) +
    #Bus
    geom_sf(data = tstg.df, color = alpha(color_tstg,0.2), size = .2) +
    #Osm
    geom_sf(data = low_stress, color = alpha(color_calles_lts_1,0.2), size = .2) +
    geom_sf(data = medium_stress, color = alpha(color_calles_lts_2,0.2), size = .3) +
    geom_sf(data = high_stress, col = alpha(color_calles_lts_3,0.2), size = .4) +
    #Cordenadas
    coord_sf(
      expand = TRUE,label_axes = element_blank())
