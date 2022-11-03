#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
#require("ggplot2")


#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("D:/FCEN/Materias/DataMining/Materias/DMEF")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv" )


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


#----------------------------------------------------------------------------
# habilitar esta seccion si el Fiscal General  Alejandro Bolaños  lo autoriza
#----------------------------------------------------------------------------

# corrijo manualmente el drifting de  Visa_fultimo_cierre
dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]

# Elimino la mejor variable
# ctrx_quarter

dapply$ctrx_quarter <- NULL
dtrain$ctrx_quarter <- NULL


# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dataset,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -1,#  -0.89
                 minsplit=  1000,   # 621
                 minbucket=  1000,   # 309
                 maxdepth=     3 )  #  12


prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

# reglas

dataset[, campo1 := as.integer(mcaja_ahorro >= 259.94 & mtarjeta_visa_consumo >= 2003.7 &
                                mpasivos_margen >= 231.5)]
dataset[, campo2 := as.integer(mcaja_ahorro >= 259.94 & mtarjeta_visa_consumo < 2003.7 &
                                mpasivos_margen < 231.5)]
dataset[, campo3 := as.integer(mcaja_ahorro >= 259.94 & mtarjeta_visa_consumo < 2003.7 &
                                 cpayroll_trx >= 1)]
dataset[, campo4 := as.integer(mcaja_ahorro >= 259.94 & mtarjeta_visa_consumo < 2003.7 &
                                 cpayroll_trx < 1)]
dataset[, campo5 := as.integer(mcaja_ahorro < 259.94 & mtarjeta_visa_consumo >= 857.2 &
                                Visa_msaldototal >= 7919.8)]
dataset[, campo6 := as.integer(mcaja_ahorro < 259.94 & mtarjeta_visa_consumo >= 857.2 &
                                Visa_msaldototal < 7919.8)]
dataset[, campo7 := as.integer(mcaja_ahorro < 259.94 & mtarjeta_visa_consumo < 857.2 &
                                mprestamos_personales >= 14.858e+3)]
dataset[, campo8 := as.integer(mcaja_ahorro < 259.94 & mtarjeta_visa_consumo < 857.2 &
                                mprestamos_personales < 14.858e+3)]

