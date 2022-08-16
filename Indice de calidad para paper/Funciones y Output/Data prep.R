# Contrato: Esta funcion recibe un data frame del catastro más ciclo
# Funcion: Esta funcion reduce los valor de las tipologias a 4 categorias CA, VD, BAND, PAR
# Si hay una ciclovia por platabanda esta funcion la transforma en una ciclovias por vereda con segregacion
#



GetDataReady <- function(df){

data_sp <- df
data_sp <-   janitor::clean_names(data_sp)
data_sp <-   dplyr::select(data_sp,  "comuna"  ,   "ci_o_cr" ,   "tip_via_ci" ,"ancho_v" ,   "t_seg_vd",
                "t_seg_ca"  , "ci_vd"   ,   "ci_par"   ,  "ci_s_par"  , "ci_ca"    ,  "otros_ci" ,  "material",
                "veg"    ,    "t_via_cr"  , "senalizad" ,"pintado"   , "semaforo"  , "cartel"  ,   "otros_cr",
                "ciclov"   ,  "discon"    , "lineas_p"  , "color_p" ,   "ancho_s"  ,  "ci_plat"  ,  "ci_band",
                 "phanto"  ,   "comp"  ,     "largo_m" ,   "shape_leng" ,"discon_cr" , "oneway"  ,   "id",
                "ciclovia_n", "pistas_via" ,"ancho_via" , "fecha_ci"  , "id_2")


data_sp <- dplyr::mutate(data_sp,
                         # Materiales == NA ~ "Sin Informacion"
                         material = case_when(
                           material == "NA" ~ "Sin Informacion",
                           material == "0" ~ "Sin Informacion",
                           material == "2" ~ "Sin Informacion",
                           material == "adoquines" ~ "ADOQ",
                           material == "PEC" ~ "PEC",
                           material == "PET" ~ "DET",
                           material == "ADOQ" ~ "ADOQ",
                           material == "DIV" ~ "DIV",
                           material == "OBS" ~ "OBS",
                           material == "VER" ~ "VER",
                           material == "DET" ~ "DET",
                           material == "INTRA" ~ "INTRA",
                           is.na(material) ~ "Sin Informacion"),

                         #Correciones CI_VD
                         ci_vd = case_when(ci_plat == 1 ~ 1, ciclov == 1 ~ 1 , TRUE ~ as.numeric(as.character(ci_vd))),
                         t_seg_vd = case_when(ci_plat == 1 ~ 1, ciclov == 1 ~ 0 , TRUE ~ as.numeric(as.character(t_seg_vd))),
                         color_p = case_when(ciclov == 1 ~ 0 , ciclovia_n == 45 ~ 3, TRUE ~ as.numeric(as.character(color_p))),
                         lineas_p = case_when(ciclov == 1 ~ 0 , TRUE ~ as.numeric(as.character(lineas_p))),

                         #Correciones CI_PAR

                         ci_par = case_when(ci_s_par == 1 ~ 1,  TRUE ~ as.numeric(as.character(ci_par))),
                         material = case_when(ci_s_par == 1 ~ "PEC"),

                         #Columna resumen

                         tipci = dplyr::case_when(
                           ci_ca == 1 ~ "CA",
                           ci_vd == 1 ~ "VD",
                           ci_par == 1 ~ "PAR",
                           ci_band == 1 ~ "BAND"
                         )

                         )
  return(data_sp)
#Resultados y tablas
}



# Function to get basic analysis (Descriptive) ----
GetBasicAnalysis <- function(df){
  # Data set Ciclovias and Cruces ----
  tabla_1a <- df %>%
    dplyr::mutate(cicr = dplyr::if_else(ci_o_cr == 1, "Ciclovia", "Cruce")) %>%
    st_drop_geometry %>%
    group_by(cicr) %>%
    summarise(
      km_total = round(sum(largo_m, na.rm = TRUE) / 1000, 2),
      # Km_pro = round(mean(largo_m, na.rm = TRUE) / 100, 2)
      largo_pro = round(mean(largo_m, na.rm = TRUE), 2)
    ) %>%
    dplyr::rename(
      `Km Totales` = km_total,
      `Promedio de Largo` = largo_pro,
      Tipo = cicr
    )

  # Write data tabla 1a
  readr::write_csv(tabla_1a, "output/data basics/1.a_km_totales_ciclovias_y_cruces.csv")


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
      km_total = round(sum(largo_m, na.rm = TRUE) / 1000, 2),
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
  readr::write_csv(tabla_1b, "output/data basics/1.b_km_totales_ciclovias_y_cruces_por_comuna.csv")

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
      km_total = round(sum(largo_m, na.rm = TRUE) / 1000, 2),
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
  readr::write_csv(tabla_2a, "output/data basics/2.a_km_totalespor_tipologia.csv")

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
      km_total = round(sum(largo_m, na.rm = TRUE) / 1000, 2),
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
  readr::write_csv(tabla_2b, "output/data basics/2.b_km_totales_por_tipologia_por_comuna.csv")


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
      km_total = round(sum(largo_m, na.rm = TRUE) / 1000, 2),
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
  readr::write_csv(tabla_3a, "output/data basics/3.akm_totales_por_tipo_de_servicio_de_calle.csv")

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
      km_total = round(sum(largo_m, na.rm = TRUE) / 1000, 2),
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
  readr::write_csv(tabla_3b, "output/data basics/3.b_km_totales_por_tipo_de_servicio_de_calle_por_comuna.csv")


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

