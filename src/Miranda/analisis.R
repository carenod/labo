#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("GGally")

#Parametros del script
PARAM <- list()
PARAM$experimento  <- "CLU1262"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd( "D:/FCEN/Materias/DataMining/Materias/DMEF/exp/Miranda/CLU1262/" ) 

#leo el dataset original
# pero podria leer cualquiera que tenga Feature Engineering
dataset  <- fread( "./cluster_de_bajas_12meses.txt", stringsAsFactors= TRUE)


ggpairs(dataset)

dataset[ , .N, cluster2]
dataset[  , mean(ctrx_quarter),  cluster2 ]  # cantidad de movimientos en la cuenta
dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(chomebanking_transacciones),  cluster2 ]
dataset[  , mean(cliente_edad),  cluster2 ] # todos los clusters tienen entre 46 y 50 años
dataset[  , mean(cliente_antiguedad),  cluster2 ] # 6 clusters 10 años, 1 cluster 4 años
dataset[  , mean(cliente_vip),  cluster2 ] # no son clientes vip
dataset[  , mean(mrentabilidad),  cluster2 ]

dataset[  , sum(cpayroll_trx)/.N*100,  cluster2 ]

dataset[  , mean(catm_trx),  cluster2 ]

dataset[  , sum(cforex),  cluster2 ]
dataset[  , sum(cforex_buy),  cluster2 ]
dataset[  , sum(cforex_sell),  cluster2 ]

dataset[  , mean(cproductos),  cluster2 ]

dataset[  , mean(cprestamos_hipotecarios),  cluster2 ]

dataset[  , mean(cprestamos_personales),  cluster2 ]


columnas <- colnames(dataset[ , .SD, .SDcols = is.numeric])
clusters <- unique(dataset$cluster2)

dfm <- data.table(clustere2 = clusters)

for (c in 1:156) {
  nc <- dataset[ , mean(columnas[c]), cluster2]
  dfm <- dfm[ , columnas[c] := nc$V1]
}

library(dplyr)

dataset %>% 
  group_by(cluster2) %>% 
  summarise_all(list(mean, sum))

dt_mean <- dataset %>% 
  group_by(cluster2) %>%
  summarise_all("mean")


fwrite( dt_mean,
        file= "dt_mean.txt",
        sep= "\t",
        dec = ",")

library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2") 

pie(c(6198, 2610), labels = NULL,col=myPalette, radius = 1)

