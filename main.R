# Autor: Ruben Dario Lopez.
# E-mail: macv1031@gmail.com.
# Github: macv1031
# Fecha: enero, 2021
#
# Descripcion: Este es el script madre que comtiene todo el proceso de limpieza de datos y funciones que
# generan graficos y tablas a partir de un archivo shape de redes de ciclovias.

# El archivo contiene 3 funciones principales :

    # 1.- GetShapeFile: Funcion que busca un archivo tipo shp en la carpeta de data y lo lee. a su vez, tambien
    #     genera un archivo csv con inconsistencias de la data y la envia a output. Estos archivos se deben revisar y limpiar
    #     manualmente para luego ser agregados al archivo shp original y correr la funcion neuvamente hasta qu eno tenga errores.

    #     Esta funcion necesita el nombre del archivo shp que se analizara. Debe ir entre comillas y con exactitud.


    # 2.- GetBasicAnalysis: Funcion que se alimenta del resultado del archivo shp ya limpio a travez de GetShapeFile.
    #     Esta funcion extrae tablas y graficos del analisis basico. los archivos salida van a la carpeta de outputs.


    # 3.- GetQualityanalysis: Funcion que a partir del archivo shp procesado con la funcion GetShapeFile, calcula un indice
    #     de calidad y lo convierte en tablas que son escritas en formato csv y graficos en la carpeta output.




# Libraries ----
library(dplyr)
library(sf)
library(janitor)
library(ggplot2)


# Data Raw -----
data_raw_sp <- sf::st_read("data/ciclovias_unidas.shp")
prueba <- sf::st_read("data/Prueba.shp")

# Function Data wrangling ----
GetShapeFile <- function(file){
# Searching file ----
df <- sf::st_read(paste0("data/", file, ".shp"))


# Auxiliar variable ----
order_col <- c("ci_ca", "ci_vd", "ciclov", "ci_plat", "ci_par", "ci_s_par", "ci_band",
            "material","t_seg_vd", "lineas_p", "color_p", "ci_o_cr")

# Reading data and wrangling ----
data_sp <- df %>%
  janitor::clean_names() %>%
  dplyr::select(comuna, order_col, "tip_via_ci", "ancho_v", "t_seg_ca", "otros_ci",
         "veg", "t_via_cr", "senalizad", "pintado", "semaforo", "cartel",
         "otros_cr", "discon", "ancho_s", "ci_band", "phanto", "comp",
         "largo_m", "shape_leng", "discon_cr", "oneway", "id", "ciclovia_n",
         "pistas_via", "ancho_via", "tipci", "fecha_ci", "id_2") %>%
  dplyr::mutate(
            ci_vd = dplyr::if_else(ci_plat == 1, 1, ci_vd),
            material = dplyr::case_when(
              # material == NA ~ "Sin Informacion",
              material == "NA" ~ "Sin Informacion",
              material == "0" ~ "Sin Informacion",
              material == "2" ~ "Sin Informacion",
              material == "adoquines" ~ "ADOQ",
              material == "PEC" ~ "PEC",
              material == "PET" ~ "PET",
              material == "ADOQ" ~ "ADOQ",
              material == "DIV" ~ "DIV",
              material == "OBS" ~ "OBS",
              material == "VER" ~ "VER",
              material == "DET" ~ "DET",
              material == "INTRA" ~ "INTRA",
              is.na(material) ~ "Sin Informacion"),
            t_seg_vd = dplyr::if_else(ci_plat == 1, 1, t_seg_vd),
            ci_vd = dplyr::if_else(ciclov == 1, 1, ci_vd),
            t_seg_vd = dplyr::if_else(ciclov == 1, 0, t_seg_vd),
            ci_par = dplyr::if_else(ci_s_par == 1, 1, ci_par),
            material = dplyr::if_else(ciclov == 1, dplyr::na_if(material, " "), material),
            lineas_p = dplyr::if_else(ciclov == 1, 0, lineas_p),
            t_seg_vd = dplyr::if_else(ciclov == 1, 0, t_seg_vd),
            color_p = dplyr::if_else(ciclov == 1, 0, color_p),
            ci_vd = dplyr::if_else(ciclov == 1, 1, ci_vd),
            material = dplyr::if_else(ci_s_par == 1, "PEC", material),
            tipci = dplyr::case_when(
              ci_ca == 1 ~ "CA",
              ci_vd == 1 ~ "VD",
              ci_par == 1 ~ "PAR",
              ci_band == 1 ~ "BAND"
              )
            )

# cleanning data ----
futile.logger::flog.info("Exploring data and filtering it")
# condition 2
if (length(data_sp != 0)) {
  df_clean <- data_sp %>%
    dplyr::filter(!phanto == 1,
                  !is.na(tipci),
                  sf::st_is_empty(.) == FALSE
    )

  futile.logger::flog.info("Completed data cleanning. Ready to use it")
}



# Condition 1
# if (data_sp$phanto == 1 | is.na(data_sp$tipci)) {
  # | sf::st_is_empty(data_sp$geometry) == TRUE
  corregir <- data_sp %>%
    dplyr::filter(phanto == 1 | is.na(tipci) | sf::st_is_empty(.) == TRUE) %>%
    mutate(error = case_when(
      phanto == 1 ~ "Phanto = 1",
      is.na(tipci) ~ "Is NA tipci variable",
      sf::st_is_empty(geometry) == TRUE ~ "Geometry is empty"
      )
    ) %>%
    readr::write_csv(file = "data/errors/errors_shape.csv")

  # if(length(corregir != 0) == TRUE){
  #  futile.logger::flog.info(paste0("Data Frame ", corregir, " has " , length(corregir), " errors"))
  #  } else {
  #  futile.logger::flog.info("The Data frame has 0 errors")
  #  }
# }

  return(df_clean)
}


# Function to get basic analysis (Descriptive) ----
GetBasicAnalysis <- function(df){
  # Data set Ciclovias and Cruces ----
  tabla_1a <- df %>%
    dplyr::mutate(cicr = dplyr::if_else(ci_o_cr == 1, "Ciclovia", "Cruce")) %>%
    st_drop_geometry %>%
    group_by(cicr) %>%
    summarise(
      km_total = round(sum(largo_m, na.rm = TRUE) / 100, 2),
      # Km_pro = round(mean(largo_m, na.rm = TRUE) / 100, 2)
      largo_pro = round(mean(largo_m, na.rm = TRUE), 2)
    ) %>%
    dplyr::rename(
      `Km Totales` = km_total,
      `Promedio de Largo` = largo_pro,
      Tipo = cicr
    )

  # Write data tabla 1a
  readr::write_csv(tabla_1a, "output/data/1.a_km_totales_ciclovias_y_cruces.csv")


  # Grafico 1.a: Distribucion de tipologias ciclovias totales (grafico de torta)
  chart_1a <- ggplot(tabla_1a,
                     aes(x = reorder(Tipo, -`Km Totales`), y = `Km Totales`, fill = Tipo))+
    geom_bar(width = 0.9, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green")) +
    # geom_text(aes(label = `Km Totales`), position = position_stack(vjust = 0.5)) +
    labs(fill = "Tipo de ciclovia",
         title = "Distribución de tipologías de ciclovías") +
    xlab("Tipologías ") +
    ylab("Kilometros Totales") +
    ggtitle("Distribución de tipologías de ciclovías") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_1a


  ggsave("output/charts/1.a_Distribución de tipologías de ciclovías.png",
         width = 35, height = 30, units = "cm")

  # Data set Ciclovia and Cruces por comuna ----
  tabla_1b <- df %>%
    dplyr::mutate(cicr = dplyr::if_else(ci_o_cr == 1, "Ciclovia", "Cruce")) %>%
    st_drop_geometry %>%
    group_by(cicr, comuna) %>%
    summarise(
      km_total = round(sum(largo_m, na.rm = TRUE) / 100, 2),
      largo_pro = round(mean(largo_m, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      `Km Totales` = km_total,
      `Promedio de Largo` = largo_pro,
      Tipo = cicr,
      Comuna = comuna
    ) %>%
    dplyr::arrange(desc(`Km Totales`))


  # Write data tabla 1b
  readr::write_csv(tabla_1b, "output/data/1.b_km_totales_ciclovias_y_cruces_por_comuna.csv")

  # Grafico 1.b: Distribucion de tipologias ciclovias totales
  chart_1b <- ggplot(tabla_1b,
                     aes(x = reorder(Comuna, -`Km Totales`), y = `Km Totales`, fill = Tipo)) +
    geom_bar(width = 0.8, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green")) +
    # geom_text(aes(label = `Km Totales`), position = position_stack(vjust = 0.5))
    labs(fill = "Tipo de ciclovia",
         title = "Distribución de tipologías de ciclovías por comunas") +
    xlab("Comunas") +
    ylab("Kilometros Totales") +
    theme(axis.text.x = element_text(angle = 90)) +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray")
    )
  chart_1b

  ggsave("output/charts/1.b_Distribución de tipologías de ciclovías por comunas.png",
         width = 35, height = 30, units = "cm")


  # Data set by Tipologia ----
  # Tipologia
  tabla_2a <- df %>%
    dplyr::mutate(cicr = dplyr::if_else(ci_o_cr == 1, "Ciclovia", "Cruce")) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(tipci) %>%
    dplyr::summarise(
      km_total = round(sum(largo_m, na.rm = TRUE) / 100, 2),
      largo_pro = round(mean(largo_m, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    ) %>%
    dplyr::rename(
      `Km Totales` = km_total,
      `Promedio de Largo` = largo_pro,
      Tipologia = tipci
    ) %>%
    dplyr::mutate(
      Tipologia = case_when(
        Tipologia == "BAND" ~ "Bandejon",
        Tipologia == "CA" ~ "Calle",
        Tipologia == "PAR" ~ "Parque",
        Tipologia == "VD" ~ "Vereda"
      )
    ) %>%
    dplyr::arrange(desc(`Km Totales`))


  # Write data tabla 1a
  readr::write_csv(tabla_2a, "output/data/2.a_km_totalespor_tipologia.csv")

  # Grafico 2.a: Distribucion de kilometros totales por tipologias de ciclovias
  chart_2a <- ggplot(tabla_2a,
                     aes(x = reorder(Tipologia, -`Km Totales`), y = `Km Totales`, fill = Tipologia)) +
    geom_bar(width = 0.9, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green")) +
    # geom_text(aes(label = `Km Totales`), position = position_stack(vjust = 0.5)) +
    labs(fill = "Tipo de ciclovia",
         title = "Distribución de kilometros de tipologías de ciclovías") +
    xlab("Tipologías") +
    ylab("Kilometros Totales") +
    theme(axis.text.x = element_text(angle = 90)) +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_2a


  ggsave("output/charts/2.a_Distribución de kilometros de tipologías de ciclovías.png",
         width = 35, height = 30, units = "cm")


  # Data set Distribucion de Tipologias de ciclovias por comuna ----
  tabla_2b <- df %>%
    dplyr::mutate(cicr = dplyr::if_else(ci_o_cr == 1, "Ciclovia", "Cruce")) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(comuna, tipci) %>%
    dplyr::summarise(
      km_total = round(sum(largo_m, na.rm = TRUE) / 100, 2),
      largo_pro = round(mean(largo_m, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    ) %>%
    dplyr::rename(
      `Km Totales` = km_total,
      `Promedio de Largo` = largo_pro,
      Tipologia = tipci,
      Comuna = comuna
    )


  # Write data tabla 2b
  readr::write_csv(tabla_2b, "output/data/2.b_km_totales_por_tipologia_por_comuna.csv")


  # Grafico 2.b: Distribucion de kilometros totales por tipologias de ciclovias por comunas
  chart_2b <- ggplot(tabla_2b,
                     aes(x = reorder(Comuna, -`Km Totales`), y = `Km Totales`, fill = Tipologia)) +
    geom_bar(width = 0.9, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green")) +
    # geom_text(aes(label = `Km Totales`), position = position_stack(vjust = 0.5))
    labs(fill = "Tipo de ciclovia",
         title = "Distribución de kilometros de tipologías de ciclovías por comunas") +
    xlab("Comunas") +
    ylab("Kilometros Totales") +
    theme(axis.text.x = element_text(angle = 90)) +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_2b


  ggsave("output/charts/2.b_Distribución de kilometros de tipologías de ciclovías por comunas.png",
         width = 35, height = 30, units = "cm")


  # Data set por tipo de servico de calle ----
  # Tipologia por comuna
  tabla_3a <- df %>%
    dplyr::mutate(cicr = dplyr::if_else(ci_o_cr == 1, "Ciclovia", "Cruce")) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(tip_via_ci) %>%
    dplyr::summarise(
      km_total = round(sum(largo_m, na.rm = TRUE) / 100, 2),
      largo_pro = round(mean(largo_m, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tip_via_ci = dplyr::case_when(
        tip_via_ci == 1 ~ "Expresa",
        tip_via_ci == 2 ~ "Troncal",
        tip_via_ci == 3 ~ "Colectora",
        tip_via_ci == 4 ~ "Servicio",
        tip_via_ci == 5 ~ "Residencial",
        tip_via_ci == 0 ~ "0", #Cuando sea igual a cero o diferente de las categorias, generar CSV de alerta
        tip_via_ci == 6 ~ "Pasaje"
      ),
      `%` = round((km_total / sum(km_total)) * 100, 2)
    ) %>%
    dplyr::filter(tip_via_ci != 0) %>%
    dplyr::rename(
      `Km Totales` = km_total,
      `Promedio de Largo` = largo_pro,
      `Tipo de ciclovias` = tip_via_ci
    )


  # Write data tabla 3a
  readr::write_csv(tabla_3a, "output/data/3.akm_totales_por_tipo_de_servicio_de_calle.csv")

  # Grafico 3.a: Distribucion de kilometros totales por tipo de ciclovia
  chart_3a <- ggplot(tabla_3a,
                     aes(x = reorder(`Tipo de ciclovias`, -`Km Totales`), y = `Km Totales`, fill = `Tipo de ciclovias`)) +
    geom_bar(width = 0.9, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green", "black", "purple", "darkblue")) +
    # geom_text(aes(label = `Km Totales`), position = position_stack(vjust = 0.5)) +
    labs(fill = "Tipo de ciclovia",
         title = "Distribución de kilometros por tipo de ciclovia") +
    xlab("Tipo de ciclovia") +
    ylab("Kilometros Totales") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))

  chart_3a


  ggsave("output/charts/3.a_Distribución de kilometros por tipo de ciclovia.png",
         width = 35, height = 30, units = "cm")

  # Data set de tipologias de ciclovias por distribucion de servicio de calle ----
  tabla_3b <- df %>%
    dplyr::mutate(cicr = dplyr::if_else(ci_o_cr == 1, "Ciclovia", "Cruce")) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(comuna, tip_via_ci) %>%
    dplyr::summarise(
      km_total = round(sum(largo_m, na.rm = TRUE) / 100, 2),
      largo_pro = round(mean(largo_m, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tip_via_ci = dplyr::case_when(
        tip_via_ci == 1 ~ "Expresa",
        tip_via_ci == 2 ~ "Troncal",
        tip_via_ci == 3 ~ "Colectora",
        tip_via_ci == 4 ~ "Servicio",
        tip_via_ci == 5 ~ "Residencial",
        tip_via_ci == 0 ~ "0",
        tip_via_ci == 6 ~ "Pasaje"
      ),
      `%` = round((km_total / sum(km_total)) * 100, 2)
    ) %>%
    dplyr::filter(tip_via_ci != 0) %>%
    dplyr::rename(
      `Km Totales` = km_total,
      `Promedio de Largo` = largo_pro,
      `Tipo de ciclovias` = tip_via_ci,
      Comuna = comuna
    )


  # Write data tabla 3b
  readr::write_csv(tabla_3b, "output/data/3.b_km_totales_por_tipo_de_servicio_de_calle_por_comuna.csv")


  # Grafico 3.b: Distribucion de kilometros totales por tipo de servicio por comunas
  chart_3b <- ggplot(tabla_3b,
                     aes(x = reorder(Comuna, -`Km Totales`), y = `Km Totales`, fill = `Tipo de ciclovias`)) +
    geom_bar(width = 0.9, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green", "black", "purple", "darkblue")) +
    # geom_text(aes(label = `Km Totales`), position = position_stack(vjust = 0.5))
    labs(fill = "Tipo de ciclovia",
         title = "Distribución de kilometros por tipo de ciclovia por comuna") +
    xlab("Comunas") +
    ylab("Kilometros Totales") +
    theme(axis.text.x = element_text(angle = 90)) +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_3b


  ggsave("output/charts/3.b_Distribución de kilometros por tipo de ciclovia por comuna.png",
         width = 35, height = 30, units = "cm")

}

# Function to get quality Analisis (Advanced) ----
GetQualityAnalysis <- function(df){
  # Index ----
  index <- df %>%
    mutate(
      tip_op_sup =
        case_when(
          (otros_ci %in% c("INTRA", "DET, OBS", "DET", "OBS")) & (ci_o_cr == 1) ~ 1,
          (ci_o_cr == 1) & (ci_ca == 1) & (oneway == 0) & (tip_via_ci >= 5) & (ancho_v < 150) ~ 1,
          (ci_o_cr == 1) & (ci_ca == 1) & ancho_v < 160 & oneway == 0 & tip_via_ci == 4 ~ 1,
          (ci_o_cr == 1) & (ci_ca == 1) &  ancho_v < 180 & oneway == 0 & tip_via_ci == 3 ~ 1,
          (ci_o_cr == 1) & (ci_ca == 1) & ancho_v < 200 & oneway == 0 & tip_via_ci == 2 ~ 1,
          (ci_o_cr == 1) & ancho_v < 160 & oneway == 0 ~ 1, #duda Por que es cero oneway y abajo es uno? Pareciera ser lo mismo
          (ci_o_cr == 1) & ancho_v < 80 & oneway == 1 ~ 1,
          TRUE ~ 0),
      tip_op_seg_ca =
        case_when(
          (ci_o_cr == 1) & (ci_ca == 1) & lineas_p == 1 & t_seg_ca == 1 ~ 1,
          (ci_o_cr == 1) & (ci_ca == 1) & tip_via_ci >= 5 & t_seg_ca < 1 ~ 1,
          (ci_o_cr == 1) & (ci_ca == 1) & tip_via_ci == 4 & t_seg_ca < 2 ~ 1,
          (ci_o_cr == 1) & (ci_ca == 1) & tip_via_ci == 3 & t_seg_ca < 3 ~ 1,
          (ci_o_cr == 1) & (ci_ca == 1) & tip_via_ci <= 3 & t_seg_ca < 4 ~ 1,
          TRUE ~ 0),
      tip_op_seg_vd =
        case_when(
          (ci_o_cr == 1) & (ci_vd == 1) & tip_via_ci >= 5 & material != "PEC" ~ 1, # duda, hay muchos valores con valor "sin Informaicon" Muchas NA reales
          (ci_o_cr == 1) & (ci_vd == 1) & tip_via_ci == 4 & (lineas_p == 0 | color_p == 0 | t_seg_vd == 0 | material != "PEC") ~ 1,
          (ci_o_cr == 1) & (ci_vd == 1) & tip_via_ci <= 3 & t_seg_vd == 0 & tip_via_ci != 1 ~ 1, #duda, se solapan las cuentas.
          (ci_o_cr == 1) & (ci_vd == 1) & ancho_v >= 200 & material == "PEC" ~ 0,
          (ci_o_cr == 1) & (ci_par == 1) & material != "PEC" ~ 1,
          (ci_o_cr == 1) & (ci_band == 1) & material != "Sin Informacion" &  material != "PEC" & (lineas_p == 0 | color_p == 0) ~ 1,
          TRUE ~ 0),
      op_cr =
        case_when(
          (ci_o_cr == 0) & t_via_cr >= 6 ~ 0,
          (ci_o_cr == 0) & t_via_cr == 5 & (senalizad %in% c(0, 1)) & (pintado %in% c(0, 1)) & cartel == 0 & semaforo == 0 ~ 1,
          (ci_o_cr == 0) & t_via_cr == 4 & (senalizad %in% c(0, 1)) & (pintado %in% c(0, 1)) & semaforo == 0 ~ 1,
          (ci_o_cr == 0) & t_via_cr == 3 & (senalizad %in% c(0, 1)) & (pintado %in% c(0, 1)) & semaforo == 0 ~ 1,
          (ci_o_cr == 0) & t_via_cr == 2 & (senalizad %in% c(0, 1)) & (pintado %in% c(0, 1)) & semaforo == 0 ~ 1,
          (ci_o_cr == 0) & t_via_cr <= 1 & (senalizad %in% c(0, 1)) & (pintado %in% c(0, 1)) & semaforo == 0 ~ 1,
          TRUE ~ 0),
      # op_ci = ifelse(sum(tip_op_seg_bd, tip_op_seg_ca, tip_op_sup) == 0, 0, 1)
      op_ci =
        case_when(
          tip_op_seg_vd == 0 & tip_op_seg_ca == 0 & tip_op_sup == 0 ~ 0,
          TRUE ~ 1
        )
    ) %>%
    dplyr::select(comuna, op_ci, tip_op_sup, tip_op_seg_ca, tip_op_seg_vd, op_cr, largo_m, id_2) %>%
    sf::st_drop_geometry()



  # Write data index
  readr::write_csv(index, "output/data/index.csv")


  # Data set by Maps ----
  inop_op_tipo <- index %>%
    mutate(
      tipo_inop =
        case_when(
          (tip_op_seg_vd == 0 & tip_op_sup == 0 & tip_op_seg_ca == 1) ~ 3,
          (tip_op_seg_vd == 1 & tip_op_sup == 0 & tip_op_seg_ca == 0) ~ 2,
          (tip_op_seg_vd == 0 & tip_op_sup == 1 & tip_op_seg_ca == 0) ~ 1,
          (tip_op_seg_vd == 0 & tip_op_sup == 0 & tip_op_seg_ca == 0) ~ 0,
          TRUE ~ 4
        )
    ) %>%
    select(comuna, id_2, tipo_inop, op_cr, op_ci, largo_m)
  futile.logger::flog.info("Completed Inop data frame")


  # Write data tabla 4
  readr::write_csv(inop_op_tipo, "output/data/4_tipo_inoperatividad.csv")
  futile.logger::flog.info("Data frame written")


  # Data set Kilometros Inoperativos ----
  tabla_5a <- inop_op_tipo %>%
    mutate(
      tipo = if_else((op_ci == 1 | op_cr == 1), "Inoperativo", "Operativo")
    ) %>%
    group_by(tipo) %>%
    summarise(
      km_total = sum(largo_m),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    )

  # Write data tabla 5a
  readr::write_csv(tabla_5a, "output/data/5.a_distibucion_km_operativos_inoperativos.csv")


  # Grafico 5.a: Distribucion de kilometros operative and inoperative
  chart_5a <- ggplot(tabla_5a,
                     aes(x = reorder(tipo, -km_total), y = km_total, fill = tipo))+
    geom_bar(width = 0.9, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green")) +
    # geom_text(aes(label = km_total), position = position_stack(vjust = 0.5)) +
  labs(fill = "Tipo de ciclovia",
       title = "Distribución de kilometros operativos e inoperativos") +
    xlab("Estado de la ciclovia") +
    ylab("Kilometros Totales") +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_5a



  ggsave("output/charts/5.a_Distribución de kilometros operativos e inoperativos.png",
         width = 35, height = 30, units = "cm")

  # Data set Kilometros Inoperativos  por comuna ----
  tabla_5b <- inop_op_tipo %>%
    mutate(
      tipo = if_else((op_ci == 1 | op_cr == 1), "Inoperativo", "Operativo")
    ) %>%
    group_by(comuna, tipo) %>%
    summarise(
      km_total = sum(largo_m),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    )


  # Write data tabla 5b
  readr::write_csv(tabla_5b, "output/data/5.b_distibucion_km_operativos_inoperativos_por_comuna.csv")

  # Grafico 5.a: Distribucion de kilometros operative and inoperative by comuna
  chart_5b <- ggplot(tabla_5b,
                     aes(x = reorder(comuna, -km_total), y = km_total, fill = tipo))+
    geom_bar(stat = "identity", position = "stack", width = 0.9,) +
    scale_fill_manual(values = c("orange","red", "blue", "green")) +
    # geom_text(aes(label = km_total), position = position_stack(vjust = 0.5)) +
    labs(fill = "Tipo de ciclovia",
         title = "Distribución de kilometros operativos e inoperativos por comuna") +
    xlab("Comunas") +
    ylab("Kilometros Totales") +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_5b

  ggsave("output/charts/5.b_Distribución de kilometros operativos e inoperativos por comuna.png",
         width = 35, height = 30, units = "cm")

  # Data set Kilometros de ciclovias Inoperativas ----
  tabla_6a <- inop_op_tipo %>%
    mutate(
      tipo = if_else(op_ci == 1, "Inoperativo", "Operativo")
    ) %>%
    group_by(tipo) %>%
    summarise(
      km_total = sum(largo_m),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    )


  # Write data tabla 6a
  readr::write_csv(tabla_6a , "output/data/6.a_distibucion_km_ciclovias_operativos_inoperativos.csv")

  # Grafico 6.a: Distribution of km cycle paths operative and inoperative
  chart_6a <- ggplot(tabla_6a,
                     aes(x = reorder(tipo, -km_total), y = km_total, fill = tipo))+
    geom_bar(width = 0.9, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green")) +
    # geom_text(aes(label = km_total), position = position_stack(vjust = 0.5)) +
  labs(fill = "Tipo de ciclovia",
       title = "Distribución de kilometros de tipo de ciclovia") +
    xlab("Tipo de ciclovia") +
    ylab("Kilometros Totales") +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_6a

  ggsave("output/charts/6.a_Distribución de kilometros de tipo de ciclovia.png", width = 35, height = 30, units = "cm")

  # Data set Kilometros de ciclovias Inoperativas por comuna ----
  tabla_6b <- inop_op_tipo %>%
    mutate(
      tipo = if_else(op_ci == 1, "Inoperativo", "Operativo")
    ) %>%
    group_by(comuna, tipo) %>%
    summarise(
      km_total = sum(largo_m),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    )


  # Write data tabla 6.b
  readr::write_csv(tabla_6b, "output/data/6.b_distibucion_km_ciclovias_operativos_inoperativos.csv")

  # Grafico 6.b: Distribucion de kilometros operative and inoperative by comuna
  chart_6b <- ggplot(tabla_6b,
                     aes(x = reorder(comuna, -km_total), y = km_total, fill = tipo)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("orange","red", "blue", "green")) +
    # geom_text(aes(label = km_total), position = position_stack(vjust = 0.5)) +
    labs(fill = "Tipo de ciclovia",
         title = "Distribución de kilometros de tipo de ciclovia por comunas") +
    xlab("Comunas") +
    ylab("Kilometros Totales") +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_6b

  ggsave("output/charts/6.b_Distribución de kilometros de tipo de ciclovia por comunas.png",
         width = 35, height = 30, units = "cm")

  # Data set Kilometros de tipos de inoperatividad ----
  tabla_7a <- inop_op_tipo %>%
    mutate(
      tipo_inoperatividad =
        case_when(
          tipo_inop == 4 ~ "Inoperatividad Doble o mas",
          tipo_inop == 3 ~ "Inoperativo por segregacion calzada",
          tipo_inop == 2 ~ "Inoperativo por segregacion vereda",
          tipo_inop == 1 ~ "Inoperativo por superficie",
          tipo_inop == 0 ~ "Operativo",
        )
    ) %>%
    group_by(tipo_inoperatividad) %>%
    summarise(
      km_total = sum(largo_m),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    )

  # Write data tabla 7a
  readr::write_csv(tabla_7a, "output/data/7.a_distibucion_km_tipo_inoperatividad.csv")



  # Grafico 7.a: Distribution of km type inoperative
  chart_7a <- ggplot(tabla_7a,
                     aes(x = reorder(tipo_inoperatividad, -km_total), y = km_total, fill = tipo_inoperatividad))+
    geom_bar(width = 0.9, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green", "yellow")) +
    # geom_text(aes(label = km_total), position = position_stack(vjust = 0.5)) +
  labs(fill = "Tipo de Inoperatividad",
       title = "Distribución de kilometros de ciclovias inoperativas") +
    xlab("Tipo de inoperatividad") +
    ylab("Kilometros Totales") +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_7a

  ggsave("output/charts/7.a_Distribución de kilometros de ciclovias inoperativas.png", width = 35, height = 30, units = "cm")

  # Data set Kilometros de tipos de inoperatividad por comunas ----
  tabla_7b <- inop_op_tipo %>%
    mutate(
      tipo_inoperatividad =
        case_when(
          tipo_inop == 4 ~ "Inoperatividad Doble o mas",
          tipo_inop == 3 ~ "Inoperativo por segregacion calzada",
          tipo_inop == 2 ~ "Inoperativo por segregacion vereda",
          tipo_inop == 1 ~ "Inoperativo por superficie",
          tipo_inop == 0 ~ "Operativo",
        )
    ) %>%
    group_by(comuna, tipo_inoperatividad) %>%
    summarise(
      km_total = sum(largo_m),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    )


  # Write data tabla 7b
  readr::write_csv(tabla_7b, "output/data/7.b_distibucion_km_tipo_inoperatividad_comuna.csv")

  # Grafico 7.b: Distribution of km type inoperative by comuna
  chart_7b <- ggplot(tabla_7b,
                     aes(x = reorder(comuna, -km_total), y = km_total, fill = tipo_inoperatividad)) +
    geom_bar(stat = "identity", position = "stack", width = 0.9) +
    scale_fill_manual(values = c("orange","red", "blue", "green", "yellow")) +
    # geom_text(aes(label = km_total), position = position_stack(vjust = 0.5)) +
    labs(fill = "Tipo de inoperatividad",
         title = "Distribución de kilometros de ciclovias inoperativas por comunas") +
    xlab("Comunas") +
    ylab("Kilometros Totales") +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_7b

  ggsave("output/charts/7.b_Distribución de kilometros de ciclovias inoperativas por comunas.png",
         width = 35, height = 30, units = "cm")

  # Data set Kilometros Inoperativos por cruces ----
  tabla_8a <- inop_op_tipo %>%
    mutate(
      tipo = if_else(op_cr == 1, "Inoperativo", "Operativo")
    ) %>%
    group_by(tipo) %>%
    summarise(
      km_total = sum(largo_m),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    )


  # Write data tabla 8a
  readr::write_csv(tabla_8a, "output/data/8.a_distibucion_km_tipo_inoperatividad_cruces.csv")


  # Grafico 8.a: Distribution of km type operativity by cross
  chart_8a <- ggplot(tabla_8a,
                     aes(x = reorder(tipo, -km_total), y = km_total, fill = tipo))+
    geom_bar(width = 0.9, stat = "identity") +
    scale_fill_manual(values = c("orange","red", "blue", "green", "yellow")) +
    # geom_text(aes(label = km_total), position = position_stack(vjust = 0.5)) +
    labs(fill = "Tipo de ciclovia",
         title = "Distribución de kilometros operatividad de cruces") +
    xlab("Tipo de Operatividad") +
    ylab("Kilometros Totales") +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_8a

  ggsave("output/charts/8.a_Distribución de kilometros operatividad de cruces.png", width = 35,
         height = 30, units = "cm")

  # Data set Kilometros Inoperativos cruces por comuna ----
  tabla_8b <- inop_op_tipo %>%
    mutate(
      tipo = if_else(op_cr == 1, "Inoperativo", "Operativo")
    ) %>%
    group_by(comuna, tipo) %>%
    summarise(
      km_total = sum(largo_m),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `%` = round((km_total / sum(km_total)) * 100, 2)
    )


  # Write data tabla 8b
  readr::write_csv(tabla_8b, "output/data/8.b_distibucion_km_tipo_inoperatividad_cruces_comuna.csv")


  # Grafico 8.b: Distribution of km type  operativity by comuna
  chart_8b <- ggplot(tabla_8b,
                     aes(x = reorder(comuna, -km_total), y = km_total, fill = tipo))+
    geom_bar(stat = "identity", position = "stack", width = 0.9) +
    scale_fill_manual(values = c("orange","red", "blue", "green", "yellow")) +
    # geom_text(aes(label = km_total), position = position_stack(vjust = 0.5)) +
    labs(fill = "Tipo Operatividad",
         title = "Distribución de kilometros de operatividad de cruces por comuna") +
    xlab("Comuna") +
    ylab("Kilometros Totales") +
    # ggtitle("Distribución de tipologías de ciclovías") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray"))
  chart_8b

  ggsave("output/charts/8.b_Distribución de kilometros de operatividad de cruces por comuna.png",
         width = 35, height = 30, units = "cm")

  index

}




# End ----



