#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("ggplot2")


#------------------------------------------------------------------------------

graficar_campo  <- function( campo )
{

  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==202105 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )

  densidad_A  <- density( dataset[ foto_mes==202103, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  densidad_B  <- density( dataset[ foto_mes==202105, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste0( campo) 
      )

  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202003", "202005"),
           col=c("blue", "red"), lty=c(1,2))

}
#------------------------------------------------------------------------------
#Aqui comienza el programa
setwd("~/buckets/b1")
setwd("D:/FCEN/Materias/DataMining/Materias/DMEF")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

dataset  <- dataset[  foto_mes %in% c( 202103, 202105 ) ]

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202103, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

# Entreno el modelo
# utilizo los mejores hiperparametros encontrados en una Bayesian Optimizationcon 5-fold Cross Validation
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dataset[ foto_mes==202103 ],  #los datos donde voy a entrenar
                 xval=         0,
                 cp=           -0.69,
                 minsplit=    870,
                 minbucket=     9,
                 maxdepth=      9)


campos_modelo  <- names( modelo$variable.importance )
campos_buenos  <- c( campos_modelo,  setdiff( colnames(dataset), campos_modelo ) )
campos_buenos  <-  setdiff(  campos_buenos,  c( "foto_mes","clase_ternaria","clase_binaria" ) )


dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/DR_03_05_DAC/", showWarnings = FALSE )
setwd("./exp/DR_03_05_DAC/")

pdf("densidades_03_05.pdf")

for( campo in  campos_buenos )
{
  cat( campo, "  " )
  
  graficar_campo( campo )
  
}  


dev.off()

##### Varibles que cambian > 0  #####

campos_no_tocar <- c("mcomisiones_mantenimiento",
  "ccomisiones_otras",
  "ccajas_consultas",
  "ctarjeta_visa_debitos_automaticos",
  "mttarjeta_visa_debitos_automaticos",
  "ctransferencias_emitidas",
  "matm_other",
  "catm_trx_other",
  "ccajas_depositos",
  "catm_trx",
  "cextraccion_autoservicio",
  "mtransferencias_recibidas",
  "ctarjeta_debito_transacciones",
  "Visa_delinquency",
  "mcajeros_propios_descuentos",
  "ctransferencias_recibidas",
  "ctarjeta_master_transacciones",
  "ccuenta_debitos_automaticos",
  "ccajeros_propios_descuento",
  "ctarjeta_visa_descuentos",
  "cforex",
  "cforex_buy",
  "mforex_buy",
  "cforex_sell",
  "mcheques_emitidos",
  "thomebanking",
  "cmobile_app_trx",
  "Master_mpagado")
campos_frank <- campos_buenos[!campos_buenos %in% campos_no_tocar]
campos_malos <- c("ccajas_otras", "Visa_mpagado", "Visa_Finiciomora")

for( campo in  campos_frank ) {
  dataset[ foto_mes == 202103, paste0(campo, '_r') := (frank(get(campo)) - 1) / (.N-1)]
  dataset[ foto_mes == 202105, paste0(campo, '_r') := (frank(get(campo)) - 1) / (.N-1)]
  
}


campos_buenos_r <- paste(campos_buenos,'r',sep="_")


pdf("densidades_03_05_r.pdf")

for( campo in  campos_buenos_r )
{
  cat( campo, "  " )
  
  graficar_campo( campo )
  
}  

dev.off()


# Despues de aplicar frank mas inspección visual 
# las siguientes variables empeoran y por lo tanto
# decido no aplicarselo:
#
# c(mcomisiones_mantenimiento,
# ccomisiones_otras,
# ccajas_consultas,
# ctarjeta_visa_debitos_automaticos,
# mttarjeta_visa_debitos_automaticos,
# ctransferencias_emitidas,
# matm_other,
# catm_trx_other,
# ccajas_depositos,
# catm_trx,
# cextraccion_autoservicio,
# mtransferencias_recibidas,
# ctarjeta_debito_transacciones,
# Visa_delinquency,
# mcajeros_propios_descuentos,
# ctransferencias_recibidas,
# ctarjeta_master_transacciones,
# ccuenta_debitos_automaticos,
# ccajeros_propios_descuento,
# ctarjeta_visa_descuentos,
# cforex,
# cforex_buy,
# mforex_buy,
# cforex_sell,
# mcheques_emitidos,
# thomebanking,
# cmobile_app_trx,
# Master_mpagado)



##### Scatter ´plot ####
#ccajas_otras
#Visa_mpagado

datawide <- dcast(dataset[foto_mes == 202103 | foto_mes == 202105, .(foto_mes, Visa_Finiciomora, numero_de_cliente)], 
                  numero_de_cliente ~ foto_mes,
                  value.var = "Visa_Finiciomora")

datawide <- na.omit(datawide) # de 165746 a 160922

colnames(datawide) <- c("numero_de_cliente", "marzo", "mayo")

ggplot(datawide, aes(x = marzo, y= mayo)) + 
  geom_point()

# Descarto las siguientes variables porque varian mucho de marzo a mayo
#
# ccajas_otras
# Visa_mpagado
# Visa_Finiciomora -> se podria corregir pero es muy poco imporante


# mcuentas_saldo

# mcaja_ahorro

# mprestamos_personales

# mcomisiones

# mcomisiones_otras

# mpayroll

# ccomisiones_mantenimiento

# mtarjeta_visa_consumo

# Visa_msaldototal

# Visa_msaldopesos

# ccajas_otras

# Visa_mpagominimo

# Master_Fvencimiento ( es toda negativa)

# chomebanking_transacciones

# mttarjeta_visa_debitos_automaticos

# matm_other

# ccallcenter_transacciones

# Visa_mconsumospesos

# Visa_mconsumototal

# Master_msaldototal

# mextraccion_autoservicio

# Visa_mpagospesos

##### Varibles que cambian > y < 0  #####
# mcuenta_corriente


##### Guardo vectores a txt #####

setwd("./labo/src/2_competencia/")
write.table(campos_frank, file = "campos_frank.txt", sep = "",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

write.table(campos_malos, file = "campos_malos.txt", sep = "",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
