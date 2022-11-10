#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")
require("lubridate")

#Aqui empieza el programa

setwd( "~/buckets/b1/" )

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- "./datasets/competencia3_2022.csv.gz"
dataset  <- fread( dataset_input )

dir.create("~/buckets/b1/exp/Miranda/")
dir.create("~/buckets/b1/exp/Miranda/tendencia/")
setwd( "~/buckets/b1/exp/Miranda/tendencia/")  #Establezco el Working Directory DEL EXPERIMENTO

# Hago tabla de BAJAS+1 por mes

meses <- unique(dataset$foto_mes)

bajas_1 <- c(0)
bajas_2 <- c(0,0)
total <- c()

for (m in meses) {
  t <- dataset[foto_mes == m, .N]
  total <- c(total, t)
}

for (m in meses[1:30]) {
  b1 <- dataset[foto_mes == m & clase_ternaria == "BAJA+1", .N] 
  bajas_1 <- c(bajas_1, b1)
}

for (m in meses[1:29]) {
  b2 <- dataset[foto_mes == m & clase_ternaria == "BAJA+2", .N] 
  bajas_2 <- c(bajas_2, b2)
}

movimientos <- data.table(meses = meses,
                          baja1 = bajas_1,
                          baja2 = bajas_2,
                          total = total)

# convert date columns
movimientos[, meses := ym(meses)]

#fracciones de bajas
movimientos[, per1 := baja1/total*100]

# Plot

ggplot(movimientos, aes(x= meses, y = bajas_1)) + 
  geom_point() + 
  geom_line()

ggplot(movimientos, aes(x= meses, y = bajas_2)) + 
  geom_point() + 
  geom_line()

ggplot(movimientos, aes(x= meses, y = per1)) + 
  geom_point()+ 
  geom_line()

ggplot(movimientos, aes(x= meses, y = total)) + 
  geom_point() + 
  geom_line()


coeff = 2e+05

ggplot(movimientos[c(16:31),], aes(x = meses)) + 
  geom_line( aes(y=total), size=2, color="blue") + 
  geom_line( aes(y=per1 * coeff), size=2, color="red") + 
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "# clientes totales",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./coeff, name=" % de bajas")
  )

ggsave("tendencia.png")



##### Veo a cuantos les pagamos

dataset  <- fread( "~/buckets/b1/datasets/competenciaFINAL_2022.csv.gz" )
dataset <- dataset[foto_mes == 202107,]

prediccion <- fread("~/buckets/b1/exp/007_991_ZZ_lightgbm/007_991_ZZ_lightgbm_02_045_11000.csv")

prediccion[dataset, on = 'numero_de_cliente', clase := i.clase_ternaria]
prediccion <- prediccion[Predicted == 1, ]
prediccion[ , ganancia := ifelse(clase == "BAJA+2", 78000, -2000 )]
sum(prediccion$ganancia)
prediccion[clase == "BAJA+2", .N]
dataset[clase_ternaria == "BAJA+2", .N]

# predecimos el 70% de los BAJA +2