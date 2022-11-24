#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require(data.table)

setwd("D:/FCEN/Materias/DataMining/Materias/DMEF")

base <- fread("./exp/005_SM_semillero/envios_100seeds/semillero_base_11000.csv")
RF <- fread("./exp/005_SM_semillero_RF/envios_100seeds/semillero_base_11000.csv")

# uno las tablas
all <- copy(base)
colnames(all) <- c("numero_de_cliente", "base")
all[RF, RF := Predicted, on="numero_de_cliente"]

# cuantos distintos hay=
all[ , equal := ifelse(base == RF, 0, 1)]
sum(all$equal) # hay 2014 distintos entre 11mil de ambos --> el 1%

2014/164935 * 100


