Merge.1 <- sf::st_read("data/Catastro_2021.shp")
print(colnames(Merge.1))


#Resumen de tipologias
#=====================


#Constructor
#===========

#Variables deseadas para el resumen
identificador <- c("CI_CA","CI_VD","CICLOV","CI_PLAT",
                   "CI_PAR","CI_S_PAR","CI_BAND","MATERIAL",
                   "T_SEG_VD","LINEAS_P","COLOR_P","CI_O_CR") #Orden de las columnas

ancho.id <- length(identificador)
ancho.merge <- ncol(Merge.1)
# largo.merge <- length(Merge.1[,1])
largo.merge <- nrow(Merge.1)
largo.merge <- Merge.1[,0] %>%
  as.data.frame()
colnum <- c()

#indentificador de numero de columna de variable

for(x in 1:ancho.id) {
 colnum <- c(colnum,which(colnames(Merge.1) == identificador[x]))
}

identificador <- rbind(identificador,colnum)

#Nuevas bases de datos segun variables

ci <- data.frame()

ci_ca <- data.frame()
ci_vd <- data.frame()
ci_par <- data.frame()
ci_band <- data.frame()
#==========================

cr <- data.frame()

cr_ca <- data.frame()
cr_vd <- data.frame()
cr_par <- data.frame()
cr_band <- data.frame()

#Col para tipologia

Merge.1 <- cbind(Merge.1, matrix(data = NA, nrow = length(Merge.1[,1]), ncol = 1))
names(Merge.1)[length(Merge.1[1,])] <- "TIPCI"

#Tabla TIPCI
TIPCI <- cbind(Merge.1[,1], matrix(data = NA, nrow = length(Merge.1[,1]), ncol = 1))
colnames(TIPCI) <- c("FID","TIPCI")

#Correcion ciclov, ci plat, y ci s par
#======================================

# Correcion ciclov #
for(y in 1:largo.merge) {
  if (Merge.1[y,colnum[3]] == 1) {
    Merge.1[y,colnum[8]] <-0
    Merge.1[y,colnum[9]] <- 0
    Merge.1[y,colnum[10]] <- 0
    Merge.1[y,colnum[11]] <- 0
    Merge.1[y,colnum[2]] <- 1
  }
}

# Correciion ciplat

for(y in 1:largo.merge) {
  if (Merge.1[y,colnum[4]] == 1) {
    Merge.1[y,colnum[9]] <- 1
    Merge.1[y,colnum[2]] <- 1
  }
}

# Correcion ci_par
for(y in 1:largo.merge) {
  if (Merge.1[y,colnum[6]] == 1) {
    Merge.1[y,colnum[9]] <- "PEC"
    Merge.1[y,colnum[5]] <- 1
  }
}


#corregir tipologias hibridas -----

#Extractor de filas sg?n tipo
#===============================

#Constructor de columna TIPCI


  for(y in 1:largo.merge) {
    if (Merge.1[y,colnum[1]] == 1) {
      Merge.1[1,length(Merge.1[1,])] <- "CA"
    }
  }

  for(y in 1:largo.merge) {
    if (Merge.1[y,colnum[2]] == 1) {
      Merge.1[y,length(Merge.1[1,])] <- "VD"
    }
  }

  for(y in 1:largo.merge) {
    if (Merge.1[y,colnum[5]] == 1) {
      Merge.1[y,length(Merge.1[1,])] <- "PAR"
    }
  }

  for(y in 1:largo.merge) {
    if (Merge.1[y,colnum[7]] == 1) {
      Merge.1[y,length(Merge.1[1,])] <- "BAND"
    }
  }

#Extractor ci y cr
#================

#Extractor para cruce

for(x in colnum[12]){
  for(y in 1:largo.merge) {
    if (Merge.1[y,x] == 0) {
      cr <- rbind(cr,Merge.1[y,])
    }
  }
}


#Extractor para ciclovia

for(x in colnum[12]){
  for(y in 1:largo.merge) {
    if (Merge.1[y,x] == 1) {
      ci <- rbind(ci,Merge.1[y,])
    }
  }
}

#Extractor tipologia ci
#======================
largo.ci <- length(ci[,1])

#Extractor para calles
for(y in 1:largo.ci) {
  if (ci[y,colnum[1]] == 1) {
    print(ci[y,colnum[1]])
      ci_ca <- rbind(ci_ca,ci[y,])
    }
}

#Extractor para veredas
  for(y in 1:largo.ci) {
    if (ci[y,colnum[2]] == 1) {
      ci_vd <- rbind(ci_vd,ci[y,])
    }
  }


#Extractor para parque
for(x in colnum[5]){
  for(y in 1:largo.ci) {
    if (ci[y,x] == 1) {
      ci_par <- rbind(ci_par,ci[y,])
    }
  }
}


#Extractor para bandejon
for(x in colnum[7]){
  for(y in 1:largo.ci) {
    if (ci[y,x] == 1) {
      ci_band <- rbind(ci_band,ci[y,])
    }
  }
}


#Exportardor a excel
#===================

write.csv(ci_ca,"C:\\Users\\Jaime\\Desktop\\Arcgis\\ci_ca.csv", row.names = FALSE)
write.csv(ci_vd,"C:\\Users\\Jaime\\Desktop\\Arcgis\\ci_vd.csv", row.names = FALSE)
write.csv(ci_par,"C:\\Users\\Jaime\\Desktop\\Arcgis\\ci_par.csv", row.names = FALSE)
write.csv(ci_band,"C:\\Users\\Jaime\\Desktop\\Arcgis\\ci_band.csv", row.names = FALSE)
write.csv(cr,"C:\\Users\\Jaime\\Desktop\\Arcgis\\cr.csv", row.names = FALSE)

#Tabla TIPCI para join related:

TIPCI[,2] <- Merge.1[,length(Merge.1[1,])]
write.csv(TIPCI,"C:\\Users\\Jaime\\Desktop\\Arcgis\\CATASTRO CICLO\\Indices y resultados excels\\TIPCI.csv", row.names = FALSE)

#Extractor tipologia cr
#=======================

largo.cr <- length(cr[,1])

#Extractor para calles
for(y in 1:largo.cr) {
  if (cr[y,colnum[1]] == 1) {
    print(cr[y,colnum[1]])
    cr_ca <- rbind(cr_ca,cr[y,])
  }
}

#Extractor para veredas
for(y in 1:largo.cr) {
  if (cr[y,colnum[2]] == 1) {
    cr_vd <- rbind(cr_vd,cr[y,])
  }
}


#Extractor para parque
for(x in colnum[5]){
  for(y in 1:largo.cr) {
    if (cr[y,x] == 1) {
      cr_par <- rbind(cr_par,cr[y,])
    }
  }
}


#Extractor para bandejon
for(x in colnum[7]){
  for(y in 1:largo.cr) {
    if (cr[y,x] == 1) {
      cr_band <- rbind(cr_band,cr[y,])
    }
  }
}
