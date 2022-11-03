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

##### arma secreta ##### 

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

##### agrego 30 canaritos ######
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


##### Multiplico variables nuevas multiplicando las mas importantes #####

var_importantes <- c('ctrx_quarter', 'active_quarter', 'cdescubierto_preacordado', 
                     'mrentabilidad', 'mcomisiones_otras', 'mcomisiones',
                     'mrentabilidad_annual', 'mcomisiones_mantenimiento', 'ccomisiones_mantenimiento',
                     'mactivos_margen')
dataset[, random_shit_1 := ctrx_quarter / (active_quarter +1)  ] 
dataset[, random_shit_2 := ctrx_quarter / (cdescubierto_preacordado +1)  ] 
dataset[, random_shit_3 := ctrx_quarter / (mrentabilidad +1)  ] 
dataset[, random_shit_4 := ctrx_quarter / (mcomisiones_otras +1)  ] 
dataset[, random_shit_5 := ctrx_quarter / (mcomisiones +1)  ] 
dataset[, random_shit_6 := ctrx_quarter / (mrentabilidad_annual +1)  ] 
dataset[, random_shit_7 := ctrx_quarter / (mcomisiones_mantenimiento +1)  ] 
dataset[, random_shit_8 := ctrx_quarter / (ccomisiones_mantenimiento +1)  ] 
dataset[, random_shit_9 := ctrx_quarter / (mactivos_margen +1)  ] 
 
dataset[, random_shit_10 := active_quarter / (cdescubierto_preacordado +1)  ] 
dataset[, random_shit_11 := active_quarter / (mrentabilidad +1)  ] 
dataset[, random_shit_12 := active_quarter / (mcomisiones_otras +1)  ] 
dataset[, random_shit_13 := active_quarter / (mcomisiones +1)  ] 
dataset[, random_shit_14 := active_quarter / (mrentabilidad_annual +1)  ] 
dataset[, random_shit_15 := active_quarter / (mcomisiones_mantenimiento +1)  ] 
dataset[, random_shit_16 := active_quarter / (ccomisiones_mantenimiento +1)  ] 
dataset[, random_shit_17 := active_quarter / (mactivos_margen +1)  ] 

dataset[, random_shit_18 := cdescubierto_preacordado / (mrentabilidad +1)  ] 
dataset[, random_shit_19 := cdescubierto_preacordado / (mcomisiones_otras +1)  ] 
dataset[, random_shit_20 := cdescubierto_preacordado / (mcomisiones +1)  ] 
dataset[, random_shit_21 := cdescubierto_preacordado / (mrentabilidad_annual +1)  ] 
dataset[, random_shit_22 := cdescubierto_preacordado / (mcomisiones_mantenimiento +1)  ] 
dataset[, random_shit_23 := cdescubierto_preacordado / (ccomisiones_mantenimiento +1)  ] 
dataset[, random_shit_24 := cdescubierto_preacordado / (mactivos_margen +1)  ] 

dataset[, random_shit_25 := mrentabilidad / (mcomisiones_otras +1)  ] 
dataset[, random_shit_26 := mrentabilidad / (mcomisiones +1)  ] 
dataset[, random_shit_27 := mrentabilidad / (mrentabilidad_annual +1)  ] 
dataset[, random_shit_28 := mrentabilidad / (mcomisiones_mantenimiento +1)  ] 
dataset[, random_shit_29 := mrentabilidad / (ccomisiones_mantenimiento +1)  ] 
dataset[, random_shit_30 := mrentabilidad / (mactivos_margen +1)  ]

dataset[, random_shit_31 := mcomisiones_otras / (mcomisiones +1)  ] 
dataset[, random_shit_32 := mcomisiones_otras / (mrentabilidad_annual +1)  ] 
dataset[, random_shit_33 := mcomisiones_otras / (mcomisiones_mantenimiento +1)  ] 
dataset[, random_shit_34 := mcomisiones_otras / (ccomisiones_mantenimiento +1)  ] 
dataset[, random_shit_35 := mcomisiones_otras / (mactivos_margen +1)  ]

dataset[, random_shit_36 := mcomisiones / (mrentabilidad_annual +1)  ] 
dataset[, random_shit_37 := mcomisiones / (mcomisiones_mantenimiento +1)  ] 
dataset[, random_shit_38 := mcomisiones / (ccomisiones_mantenimiento +1)  ] 
dataset[, random_shit_39 := mcomisiones / (mactivos_margen +1)  ]

dataset[, random_shit_40 := mrentabilidad_annual / (mcomisiones_mantenimiento +1)  ] 
dataset[, random_shit_41 := mrentabilidad_annual / (ccomisiones_mantenimiento +1)  ] 
dataset[, random_shit_42 := mrentabilidad_annual / (mactivos_margen +1)  ]

dataset[, random_shit_43 := mcomisiones_mantenimiento / (ccomisiones_mantenimiento +1)  ] 
dataset[, random_shit_44 := mcomisiones_mantenimiento / (mactivos_margen +1)  ]

dataset[, random_shit_45 := ccomisiones_mantenimiento / (mactivos_margen +1)  ]


# num = 1
# for (var in 1:9){
#   for (var_2 in 2:10) {
#     print(var_importantes[var])
#     dataset$var_importantes[var] <- as.numeric(dataset$var_importantes[var])
#     dataset$var_importantes[var_2] <- as.numeric(dataset$var_importantes[var_2])
#     dataset[, paste0('random_shit_', num) := var_importantes[var_2] / (var_importantes[var] +1) ] 
#     num = num + 1
#   }
# }

##### binneo por ranking #####

vars_to_bin <- c('cliente_edad', 'cliente_antiguedad',
                 'mrentabilidad', 'mrentabilidad_annual',
                 'mcomisiones', 'mactivos_margen',
                 'mpasivos_margen', 'cproductos',
                 'mcaja_ahorro', 'mcaja_ahorro_dolares',
                 'mcuentas_saldo',
                 'mautoservicio', 'mtarjeta_visa_consumo',
                 'mtarjeta_master_consumo', 'mpayroll',
                 'mcuenta_debitos_automaticos', 'mttarjeta_visa_debitos_automaticos',
                 'mpagomiscuentas', 'mcomisiones_mantenimiento',
                 'ccomisiones_otras', 'mcomisiones_otras',
                 'mtransferencias_recibidas', 'mextraccion_autoservicio',
                 'chomebanking_transacciones')
# 
# N_train <- 161342
# N_apply <- 162900
# for (var in vars_to_bin) {
#   #dataset[foto_mes==202101, paste0(var, '_rank') := (frank(var)-1)/(N_train-1)]
#   #dataset[foto_mes==202103, paste0(var, '_rank') := (frank(var)-1)/(N_apply-1)]
#   dataset$var <-(frank(dataset$var)-1)/(N_train-1)
# }
# 
# var<-'cliente_edad'
# dataset$cliente_edad

require("Hmisc")



for( campo in  vars_to_bin )
{
  if(  dataset[ , length( unique( get(campo) ) ) > 100 ] )
  {
    dataset[  , paste0( campo, "_bin" ) := as.integer( cut2(  dataset[ , get(campo) ], m=1, g=31) ) ]
    dataset[  , paste0( campo ) := NULL ]
  }
  cat( campo, " " )
}

##### separao en train y test #####

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#dataset$foto_mes <- factor(dataset$foto_mes)

##### defino variables continuas #####
# continuous_features = c('cliente_edad', 'cliente_antiguedad',
#                         'mrentabilidad', 'mrentabilidad_annual',
#                         'mcomisiones', 'mactivos_margen',
#                         'mpasivos_margen', 'cproductos',
#                         'mcaja_ahorro', 'mcaja_ahorro_dolares',
#                         'mcuentas_saldo', 'ctarjeta_debito_transacciones',
#                         'mautoservicio', 'mtarjeta_visa_consumo',
#                         'mtarjeta_master_consumo', 'mprestamos_personales',
#                         'mprestamos_hipotecarios', 'mpayroll',
#                         'mcuenta_debitos_automaticos', 'mpagomiscuentas',
#                         'mcomisiones_mantenimiento', 'mcomisiones_otras',
#                         'ccomisiones_otras', 'mtransferencias_recibidas',
#                         'mextraccion_autoservicio', 'chomebanking_transacciones',
#                         'Visa_fultimo_cierre')


##### data drifting #####

#Hago dataset long con varables continuas
dataset_cont = dataset[ , .SD, .SDcols = is.numeric]
dataset_cont_long = melt(dataset, id.vars = c("foto_mes", "numero_de_cliente"),
                    measure.vars = continuous_features)

# plot de todas las variables
ggplot(data = dataset_cont, aes(x = value, color=foto_mes)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
# solo Visa_fultimo_cierre parece tener data_drifting

# plot de variables individuales
ggplot(data = dataset, aes(x = ccomisiones_otras, color=foto_mes)) + 
  stat_density() 

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





# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.145,#  -0.89
                 minsplit=  5886,   # 621
                 minbucket=  133,   # 309
                 maxdepth=     30 )  #  12

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo$frame[ modelo$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo, -666 )

#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo_pruned,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(298043)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


dir.create( "./exp/" )
dir.create( "./exp/ultimoDia_PrimeraCompetencia_last" )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000, 15000, 20000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/ultimoDia_PrimeraCompetencia_last/KA_",  corte, ".csv"),
           sep=  "," )
}

modelo$variable.importance
