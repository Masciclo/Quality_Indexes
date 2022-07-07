#Correr extractor de tipologia hasta y incluyendo la seccion de correciones

#Constructor

#Variables deseadas para el resumen
identificador <- c("TIP_VIA_CI","CI_CA","ANCHO_V","ANCHO_S","ONEWAY","CI_VD","CI_PAR","MATERIAL","CICLOVIA_N","T_SEG_CA","T_SEG_VD","CI_BAND","LINEAS_P","COLOR_P","CI_O_CR","T_VIA_CR","SE?ALIZAD","PINTADO","CARTEL","SEMAFORO","ONEWAY","FID","Shape_Leng","COMUNA","OTROS_CI")

ancho.id <- length(identificador)
ancho.merge <- ncol(Merge.1)
largo.merge <- length(Merge.1[,1])
colnum <- c()


#indentificador de numero de columna de variable

for(x in 1:ancho.id) {
  colnum <- c(colnum,which(colnames(Merge.1)== identificador[x]))
}

identificador <- rbind(identificador,colnum)


#Tabla de evaluacion de ciclovia por segmento
indi_op <-  cbind(Merge.1[,1], Merge.1[,"CICLOVIA_N"])
largo <- length(Merge.1[,1])
indi_op <-  cbind(indi_op, matrix(data = NA, nrow = largo, ncol = 12))
indi_op[,13] <- Merge.1[,length(Merge.1[1,])]
indi_op[,14] <- Merge.1[,41]
colnames(indi_op) <- c("FID","CICLOVIA_N","OP_CI","OP_CR","PCENT_BUE_CI","PCENT_MAL_CI","RATIO.BUE_MAL_CI","PCENT_BUE_COMU","PCENT_MAL_COMU","RATIO.BUE_MAL_COMU", "ERROR_DESC", "TIP_ERROR","TIPCI","ID_2")


#Columnas con variables
TIP_VIA <- as.integer(identificador[2,1])
CI_CA <- as.integer(identificador[2,2])
ANCHO_V <- as.integer(identificador[2,3])
ANCHO_S <- as.integer(identificador[2,4])
ONEWAY <- as.integer(identificador[2,5])
CI_VD <- as.integer(identificador[2,6])
CI_PAR <- as.integer(identificador[2,7])
MATERIAL <- as.integer(identificador[2,8])
CICLOVIA_N <- as.integer(identificador[2,9])
T_SEG_CA <- as.integer(identificador[2,10])
T_SEG_VD <- as.integer(identificador[2,11])
CI_BAND <- as.integer(identificador[2,12])
LINEAS_P <- as.integer(identificador[2,13])
COLOR_P <-as.integer(identificador[2,14])
CI_O_CR <-as.integer(identificador[2,15])
TIP_VIA_CR <-as.integer(identificador[2,16])
SE?ALIZAD <-as.integer(identificador[2,17])
PINTADO <-as.integer(identificador[2,18])
CARTEL <-as.integer(identificador[2,19])
SEMAFORO <-as.integer(identificador[2,20])
ONEWAY  <- as.integer(identificador[2,21])
FID <- as.integer(identificador[2,22])
LARGO <- as.integer(identificador[2,23])
COMU <- as.integer(identificador[2,24])
OTROS_CI <- as.integer(identificador[2,25])


# verbalizador de errores
inicio <- "Errores:"
c.auto <- 'Conflicto traffico motorizado'
c.peaton <- "Conflicto con peatones"
s.desplaza <- "Superficie inadecuada"
obs <- "Obstaculizada"

#                                                 INDICE DE OPERATIVDAD:
#                                                 ======================



for (x in 1:largo) {

#1) OBSTACULOS:
#=============

  if ((Merge.1[x,OTROS_CI] == "INTRA") |(Merge.1[x,OTROS_CI] == "DET, OBS") |(Merge.1[x,OTROS_CI] == "DET") |(Merge.1[x,OTROS_CI] == "OBS")  ) {
    if (Merge.1[x,CI_O_CR] == 1) {
    indi_op[x,3] <- 0
    } else if (Merge.1[x,CI_O_CR] == 0) {
      indi_op[x,4] <- 0

    }
    #verbalizacion de la evaluacion
    #------------------------------
    indi_op[x,12] <- obs

    if (is.na(indi_op[x,11])) {
      indi_op[x,11] <- paste(inicio, Merge.1[x,OTROS_CI] )
      indi_op[x,12] <- obs
    } else {
      indi_op[x,11] <- paste(indi_op[x,11],"; obstaculizada")
      indi_op[x,12] <- obs
    }
    #----------------------------

  }
#2) CICLOVIAS:
#============

    if (Merge.1[x,CI_O_CR] == 1) {
# a) Criterio de ancho minimo
#============================

    # ??? Ancho CI CA ???
    #????????????????????????????????????????????????
    if (Merge.1[x,CI_CA] == 1) {

    # ??? Two ways ???
    # ?????????????????????????????????
      # ??? Ancho CI twoway calzadas en calles residenciales
      #---------------------------------------------
      if (Merge.1[x,ANCHO_V] < 150 && Merge.1[x,ONEWAY] == 0 && Merge.1[x,TIP_VIA] >= 5) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- s.desplaza
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Ancho insuficiente" )
          indi_op[x,12] <- s.desplaza
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Ancho insuficiente")
          indi_op[x,12] <- s.desplaza
        }
        #----------------------------
        next
      }
      #---------------------------------------------

      # ??? Ancho Ci twoway calzadas en calles servicio
      #---------------------------------------------
      if (Merge.1[x,ANCHO_V] < 160 && Merge.1[x,ONEWAY] == 0 && Merge.1[x,TIP_VIA] == 4) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- s.desplaza
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Ancho insuficiente" )
          indi_op[x,12] <- s.desplaza
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Ancho insuficiente")
          indi_op[x,12] <- s.desplaza
        }
        #----------------------------
        next
      }
      #---------------------------------------------

      # ??? Ancho Ci twoway calzadas en calles colectora
      #---------------------------------------------
      if (Merge.1[x,ANCHO_V] < 180 && Merge.1[x,ONEWAY] == 0 && Merge.1[x,TIP_VIA] == 3) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- s.desplaza
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Ancho insuficiente" )
          indi_op[x,12] <- s.desplaza
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Ancho insuficiente")
          indi_op[x,12] <- s.desplaza
        }
        #----------------------------
        next
      }
      #---------------------------------------------

      # ??? Ancho Ci twoway calzadas en calles troncal
      #---------------------------------------------
      if (Merge.1[x,ANCHO_V] < 200 && Merge.1[x,ONEWAY] == 0 && Merge.1[x,TIP_VIA] <= 2) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- s.desplaza
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Ancho insuficiente" )
          indi_op[x,12] <- s.desplaza
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Ancho insuficiente")
          indi_op[x,12] <- s.desplaza
        }
        #----------------------------
        next
      }
      #---------------------------------------------
    } else {
    c.peaton

    # ???  Ancho One way general ???
    # ???????????????????????????????????????????????????????????????????????????
      # ??? Ancho CI oneway calzadas en calles residenciales
      #---------------------------------------------------
      if (Merge.1[x,ANCHO_V] <= 80 && Merge.1[x,ONEWAY] == 1) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- s.desplaza
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Ancho insuficiente" )
          indi_op[x,12] <- s.desplaza
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Ancho insuficiente")
          indi_op[x,12] <- s.desplaza
        }
        #----------------------------
        next
      }
      #---------------------------------------------------


    # ??? Ancho Otros TIP CI ???
    #??????????????????????????????????????????????????????????????????
      # ??? Ancho CI twoway
      #------------------
      if (Merge.1[x,ANCHO_V] <= 160 && Merge.1[x,ONEWAY] == 0) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- s.desplaza
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Ancho insuficiente" )
          indi_op[x,12] <- s.desplaza
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Ancho insuficiente")
          indi_op[x,12] <- s.desplaza
        }
        #----------------------------
        next
      }
      #------------------
    }

# b) Segregaciones y demarcaciones:
#=================================

    if (Merge.1[x,CI_CA] == 1) {

      # Criterio de segregaci?n visual borrada
      #---------------------------------------
      if(Merge.1[x,LINEAS_P] == 1 && Merge.1[x,T_SEG_CA] == 1) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- c.auto
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Segregaci?n insuficiente" )
          indi_op[x,12] <- c.auto
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Segregaci?n insuficiente")
          indi_op[x,12] <- c.auto
        }
        #----------------------------
      }
      #---------------------------------------

      #??? CI_CA ???
      #???????????????????????????
      # Criterio de segregaci?n para calles residenciales
      #--------------------------------------------------
      if (Merge.1[x,TIP_VIA] >= 5 && Merge.1[x,T_SEG_CA] < 1) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- c.auto
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Segregaci?n insuficiente" )
          indi_op[x,12] <- c.auto
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Segregaci?n insuficiente")
          indi_op[x,12] <- c.auto
        }
        #----------------------------
      }
      #--------------------------------------------------

      # Criterio de segregaci?n para calles servicio
      #--------------------------------------------------
      if (Merge.1[x,TIP_VIA] == 4 && Merge.1[x,T_SEG_CA] < 2) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- c.auto
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Segregaci?n insuficiente" )
          indi_op[x,12] <- c.auto
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Segregaci?n insuficiente")
          indi_op[x,12] <- c.auto
        }
        #----------------------------
      }
      #--------------------------------------------------

      # Criterio de segregaci?n para calles colectorta
      #--------------------------------------------------
      if (Merge.1[x,TIP_VIA] == 3 && Merge.1[x,T_SEG_CA] < 3 ) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- c.auto
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Segregaci?n insuficiente" )
          indi_op[x,12] <- c.auto
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Segregaci?n insuficiente")
          indi_op[x,12] <- c.auto
        }
        #----------------------------
      }
      #--------------------------------------------------

      # Criterio de segregaci?n para calles colectorta
      #--------------------------------------------------
      if (Merge.1[x,TIP_VIA] <= 2 && Merge.1[x,T_SEG_CA] < 4) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- c.auto
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Segregaci?n insuficiente" )
          indi_op[x,12] <- c.auto
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Segregaci?n insuficiente")
          indi_op[x,12] <- c.auto
        }
        #----------------------------
      }
      #--------------------------------------------------
    }
      #??? CI_VD ???
      #???????????????????????????

    if (Merge.1[x,CI_VD] == 1) {

      # Criterio de segregaci?n para calles residenciales
      #--------------------------------------------------
      if (Merge.1[x,TIP_VIA] >= 5 && Merge.1[x,MATERIAL] != 'PEC') {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- s.desplaza
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Superficie de rodado inadecuada" )
          indi_op[x,12] <- s.desplaza
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Superficie de rodado inadecuada")
          indi_op[x,12] <- s.desplaza
        }
        #----------------------------
      }
      #--------------------------------------------------

      # Criterio de segregaci?n para calles servicio
      #--------------------------------------------------
      if (Merge.1[x,TIP_VIA] == 4 && ((Merge.1[x,LINEAS_P] == 0 || Merge.1[x,COLOR_P] == 0 || Merge.1[x,T_SEG_VD] == 0) || Merge.1[x,MATERIAL] != 'PEC')) {
        indi_op[x,3] <- 0

        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- c.peaton
        if (Merge.1[x,LINEAS_P] == 0 || Merge.1[x,COLOR_P] == 0 || Merge.1[x,T_SEG_VD] == 0){
          verbo <- "Demarcacion inadecuada"
        } else if (Merge.1[x,MATERIAL] != 'PEC'){
          verbo <- "Superficie de rodado inadecuada"
        } else if (((Merge.1[x,LINEAS_P] == 0 || Merge.1[x,COLOR_P] == 0 || Merge.1[x,T_SEG_VD] == 0) && Merge.1[x,MATERIAL] != 'PEC')) {
          verbo <- "Demarcacion inadecuada ; Superficie de rodado inadecuada"
        }
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio, verbo)

        } else {
          indi_op[x,11] <- paste(indi_op[x,11],";", verbo)

        }
        #----------------------------
      }
      #--------------------------------------------------

      # Criterio de segregaci?n para calles colectora
      #--------------------------------------------------
      if (Merge.1[x,TIP_VIA] <= 3 && Merge.1[x,T_SEG_VD] == 0 && Merge.1[x,TIP_VIA] != 1) {
        indi_op[x,3] <- 0
        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- c.peaton
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Segregaci?n insuficiente")

        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Segregaci?n insuficiente")

        }
        #----------------------------
      }
      if (Merge.1[x,ANCHO_V] >= 200 && Merge.1[x,MATERIAL] == 'PEC') {
        indi_op[x,3] <- 1
        indi_op[x,12] <- NA
      }
      #--------------------------------------------------
    }


      #??? CI_BAND ???
      #??????????????????????????????
    #Criterio de segregaci?n para calles residenciales
    #--------------------------------------------------
    if (Merge.1[x,CI_BAND] == 1) {
      if (Merge.1[x,MATERIAL] != 'PEC' && (Merge.1[x,LINEAS_P] == 0 || Merge.1[x,COLOR_P] == 0)  ) {
        indi_op[x,3] <- 0
        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- c.peaton
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Demarcacion inadecuada y superficie de rodado inadecuadas")

        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Demarcacion inadecuada y superficie de rodado inadecuadas")

        }
        #----------------------------
      }
    }
    #--------------------------------------------------

      #??? CI_PAR ???
      #???????????????????????????

    #Criterio de segregaci?n para calles residenciales
    #--------------------------------------------------
    if (Merge.1[x,CI_PAR] == 1) {
      if (Merge.1[x,MATERIAL] != 'PEC')  {
        indi_op[x,3] <- 0
        #verbalizacion de la evaluacion
        #------------------------------
        indi_op[x,12] <- c.peaton
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Superficie de rodado inadecuada")

        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Superficie de rodado inadecuada")

        }
        #----------------------------
      }
    }
    #--------------------------------------------------
  }
  else if (Merge.1[x,CI_O_CR] == 0) {
    indi_op[x,3] <- 'cr'
  }

  for (x in 1:largo) {

    if (is.na(indi_op[x,3])) {
      indi_op[x,3] <- 1


    }
  }
}



#3) Indice de operatividdad para cruces
#======================================

for (x in 1:largo) {

  if (Merge.1[x,CI_O_CR] == 0) {

    if (Merge.1[x,TIP_VIA_CR] >= 6) {
      indi_op[x,4] <- 1
      #verbalizacion de la evaluacion
      #------------------------------
      indi_op[x,11] <- paste("Cruce en buen estado")
      #----------------------------
    }
    if (Merge.1[x,TIP_VIA_CR] == 5) {
      if ((Merge.1[x,SE?ALIZAD] == 0 || Merge.1[x,SE?ALIZAD] == 1) && (Merge.1[x,PINTADO] == 0 || Merge.1[x,PINTADO] == 1) && Merge.1[x,CARTEL] == 0 && Merge.1[x,SEMAFORO] == 0) {
        indi_op[x,4] <- 0
        #verbalizacion de la evaluacion
        #------------------------------
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Ausencia de se?alizaci?n")
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Ausencia de se?alizaci?n")
        }
        #----------------------------
      } else {
        indi_op[x,4] <- 1
      }
    }
    if (Merge.1[x,TIP_VIA_CR] == 4) {
      if ((Merge.1[x,SE?ALIZAD] == 0 || Merge.1[x,SE?ALIZAD] == 1) && (Merge.1[x,PINTADO] == 0 || Merge.1[x,PINTADO] == 1) && Merge.1[x,SEMAFORO] == 0) {
        indi_op[x,4] <- 0
        #verbalizacion de la evaluacion
        #------------------------------
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Se?alizaci?n insuficiente")
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Se?alizaci?n insuficiente")
        }
        #----------------------------
      } else {
        indi_op[x,4] <- 1
      }
    }
    if (Merge.1[x,TIP_VIA_CR] == 3) {
      if ((Merge.1[x,SE?ALIZAD] == 0 || Merge.1[x,SE?ALIZAD] == 1) && (Merge.1[x,PINTADO] == 0 || Merge.1[x,PINTADO] == 1) && Merge.1[x,SEMAFORO] == 0) {
        indi_op[x,4] <- 0
        #verbalizacion de la evaluacion
        #------------------------------
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Se?alizaci?n insuficiente")
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Se?alizaci?n insuficiente")
        }
        #----------------------------
      } else {
        indi_op[x,4] <- 1
      }
    }
    if (Merge.1[x,TIP_VIA_CR] == 2)  {
      if (((Merge.1[x,SE?ALIZAD] == 0 || Merge.1[x,SE?ALIZAD] == 1)  && Merge.1[x,SEMAFORO] == 0) || ((Merge.1[x,PINTADO] == 0 || Merge.1[x,PINTADO] == 1) && Merge.1[x,SEMAFORO] == 0)) {
        indi_op[x,4] <- 0
        #verbalizacion de la evaluacion
        #------------------------------
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Se?alizaci?n insuficiente")
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Se?alizaci?n insuficiente")
        }
        #----------------------------
      } else {
        indi_op[x,4] <- 1
      }
    }
    if (Merge.1[x,TIP_VIA_CR] <= 1)  {
      if ((Merge.1[x,SE?ALIZAD] == 0 || Merge.1[x,SE?ALIZAD] == 1) || (Merge.1[x,PINTADO] == 0 || Merge.1[x,PINTADO] == 1) || Merge.1[x,SEMAFORO] == 0) {
        indi_op[x,4] <- 0
        #verbalizacion de la evaluacion
        #------------------------------
        if (is.na(indi_op[x,11])) {
          indi_op[x,11] <- paste(inicio,"Se?alizaci?n insuficiente")
        } else {
          indi_op[x,11] <- paste(indi_op[x,11],"; Se?alizaci?n insuficiente")
        }
        #----------------------------
      } else {
        indi_op[x,4] <- 1
      }
    }
  }
  else if (Merge.1[x,CI_O_CR] == 1) {
    indi_op[x,4] <- "ci"
  }


  for (x in 1:largo) {

    if (is.na(indi_op[x,4])) {
      indi_op[x,4] <- 1
    }
  }
}


#verbalizacion de la evaluacion
#------------------------------
for (x in 1:largo) {


    if (indi_op[x,3] == "cr" & indi_op[x,4] == 1) {
      indi_op[x,11] <- "Cruce en buen estado"
    }
    if(indi_op[x,4] == "ci" & indi_op[x,3] == 1) {
      indi_op[x,11] <- "Segmento en buen estado"
    }
  }
if (is.na(indi_op[x,11])) {
  indi_op[x,11] <- "No evaluado"
}
#----------------------------


#Porcentaje de inoperativo en ciclovias
#======================================
#======================================


indi.CICLO_N <- unique(indi_op[,2])

for(x in indi.CICLO_N) {

  indi.ci <- 0
  bue <- 0
  mal <- 0
  porcent.ci_bue <- 0
  porcent.ci_mal <- 0

  for(y in 1:largo) {

    if(indi_op[y,2] == x) {

      indi.ci <- indi.ci + Merge.1[y,LARGO]

      #Que calidad debe mirar el programa
      if (indi_op[y,4] == "ci") {
        if (indi_op[y,3] == 0) {
          mal <- mal + Merge.1[y,LARGO]
        } else if (indi_op[y,3] == 1) {
          bue <- bue + Merge.1[y,LARGO]
        }
      }
      if (indi_op[y,3] == "cr") {
        if (indi_op[y,4] == 0) {
          mal <- mal + Merge.1[y,LARGO]
        } else if (indi_op[y,4] == 1) {
          bue <- bue + Merge.1[y,LARGO]
        }
      }
    }
  }
  #Caculo del porcentage del N_CICLo


  porcent.ci_bue <- bue/indi.ci
  porcent.ci_mal <- mal/indi.ci
  ratio.bue_mal<- bue/mal


  #Etiquetando el valor del porcentaje a cada segmento de la ciclovia

  for(y in 1:largo) {
    if (indi_op[y,2] == x) {
      indi_op[y,5] <- round(porcent.ci_bue,2)
      indi_op[y,6] <- round(porcent.ci_mal,2)
      indi_op[y,7] <- round(ratio.bue_mal,2)
    }
  }
}


#Porcentaje de inoperativo en Comuna
#======================================
#======================================

## Tablas de comunas
##====================

indi_op_comun <- matrix(data = NA, nrow = 5, ncol = length(unique(Merge.1[,COMU])))

colnames(indi_op_comun) <- unique(Merge.1[,COMU])
row.names(indi_op_comun) <- c("Porcentage Operativas", "Porcentage Inoperativas", "Ratio operativas/inoperativas","km Operativos", "km Inoperativos")

## Porcentaje por shape

indi.COMU <- unique(Merge.1[,COMU])

for(x in indi.COMU) {

  indi.ci <- 0
  bue <- 0
  mal <- 0
  porcent.ci_bue <- 0
  porcent.ci_mal <- 0

  for(y in 1:largo) {

    if(Merge.1[y,COMU] == x) {

      indi.ci <- indi.ci + Merge.1[y,LARGO]

      #Que calidad debe mirar el programa
      if (indi_op[y,4] == "ci") {
        if (indi_op[y,3] == 0) {
          mal <- mal + Merge.1[y,LARGO]
        } else if (indi_op[y,3] == 1) {
          bue <- bue + Merge.1[y,LARGO]
        }
      }
      if (indi_op[y,3] == "cr") {
        if (indi_op[y,4] == 0) {
          mal <- mal + Merge.1[y,LARGO]
        } else if (indi_op[y,4] == 1) {
          bue <- bue + Merge.1[y,LARGO]
        }
      }
    }
  }
  #Caculo del porcentage del N_CICLo


  porcent.ci_bue <- bue/indi.ci
  porcent.ci_mal <- mal/indi.ci
  ratio.bue_mal<- bue/mal


  #Etiquetando el valor del porcentaje a cada segmento de la ciclovia

  for(y in 1:largo) {
    if (Merge.1[y,COMU] == x) {
      indi_op[y,8] <- round(porcent.ci_bue,2)
      indi_op[y,9] <- round(porcent.ci_mal,2)
      indi_op[y,10] <- round(ratio.bue_mal,2)
    }
  }

  indi_op_comun[1,x] <- round(porcent.ci_bue,2)
  indi_op_comun[2,x] <- round(porcent.ci_mal,2)
  indi_op_comun[3,x] <- round(ratio.bue_mal,2)
  indi_op_comun[4,x] <- round(bue,2)
  indi_op_comun[5,x] <- round(mal,2)
}

write.csv(indi_op,"C:\\Users\\Jaime\\Desktop\\Arcgis\\CATASTRO CICLO\\Indices y resultados excels\\indi_op.csv", row.names = FALSE)
write.csv(indi_op_comun,"C:\\Users\\Jaime\\Desktop\\Arcgis\\CATASTRO CICLO\\Indices y resultados excels\\indi_op_comun.csv", row.names = TRUE)

