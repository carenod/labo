#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

library(data.table)

setwd("D:/FCEN/Materias/DataMining/Materias/DMEF/")

PARAM <- list()
PARAM$base <- "./exp/005_SM_semillero_RF/"
PARAM$cortes  <- seq( from=  7000,
                      to=    14000,
                      by=    1000 )

##### Cargo los archivos #####
archivos <- list.files(path = PARAM$base, pattern = "semillero", full.names = TRUE)

for (archivo in archivos) {
  
  ksemilla <- strtoi(sapply(strsplit(archivo, "_"), "[", 8))
  
  # cols: numero_de_cliente,foto_mes,prob,rank
  tb_prediccion <- fread(archivo)
  
  if (archivos[1] == archivo) {
    tb_ranking_semillerio <- data.table(numero_de_cliente = tb_prediccion[, numero_de_cliente])
  }
  
  # repara bug en z1292, si se fixea ahi, esto no genera problemas
  tb_prediccion[, rank := frank(rank, ties.method = "random")]
  
  # Generamos predicción del semillerio
  tb_ranking_semillerio[tb_prediccion, 
                        paste0("rank_", ksemilla) := rank,
                        on = "numero_de_cliente"]
  
  
}

col_rank <- grep("rank", colnames(tb_ranking_semillerio), value=TRUE)
col_pred <- c("numero_de_cliente", "Predicted")

tb_ranking_semillerio[, media := rowMeans(tb_ranking_semillerio[,..col_rank])]

setorder(tb_ranking_semillerio, media) # Esto es un ranking, entonces de menor a mayor

setwd(paste0(PARAM$base, "/envios_100seeds"))
for (corte in PARAM$cortes)
{
  tb_ranking_semillerio[, Predicted := 0]
  tb_ranking_semillerio[1:corte, Predicted := 1L]
  tb_prediccion_semillerio <- tb_ranking_semillerio[,..col_pred]
  fwrite(tb_prediccion_semillerio, paste0("semillero_base_", corte, ".csv"))
}

# comando kaggle

# ls | grep "^semillero" | while read -r line ; do kaggle competitions submit -c dm-eyf-2022-ultima -f $line -m "RF"; done

# kaggle competitions submit -c dm-eyf-2022-ultima -f semillero_base_11000.csv -m "RF 100 seeds"