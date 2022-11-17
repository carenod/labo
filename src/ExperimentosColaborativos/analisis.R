#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

library(data.table)
library(ggplot2)
library(stringr)
library(reshape2)

setwd("D:/FCEN/Materias/DataMining/Materias/DMEF/")

PARAM <- list()
PARAM$original <- "./exp/EC_ZZ9410_semillerio_replica_original/"
PARAM$no_drifting <- "./exp/EC_ZZ9410_semillero_no_drifting/"
# PARAM$FE <- "./exp/EC_ZZ9410_semillerio_FE_JIG/"
PARAM$ventana6 <- "./exp/EC_ZZ9410_ventana6/"
PARAM$no_2019 <- "./exp/EC_ZZ9410_semillero_no_2019/"
PARAM$trece_meses <- "./exp/EC_ZZ9410_semillero_trece_meses/"

PARAM$respuesta <- "./datasets/exp_EC_ZZ9410_semillerio_replica_original_clase202107.csv"

##### Cargo resultados #####
rta <- fread(PARAM$respuesta)

##### Cargo los archivos #####
archivos_original <- list.files(path = PARAM$original, pattern = "^prob_", full.names = TRUE)
archivos_no_drifting <- list.files(path = PARAM$no_drifting, pattern = "^prob_", full.names = TRUE)
# archivos_FE <- list.files(path = PARAM$FE, pattern = "^prob_", full.names = TRUE)
archivos_ventana6 <- list.files(path = PARAM$ventana6, pattern = "^prob_", full.names = TRUE)
archivos_no_2019 <- list.files(path = PARAM$no_2019, pattern = "^prob_", full.names = TRUE)
archivos_trece_meses <- list.files(path = PARAM$trece_meses, pattern = "^prob_", full.names = TRUE)



##### Funcion que hace ensamble #####
makeEnsemble <- function(archivos, rta) {
  for (f in archivos) {
    semilla <- str_extract(f, "_[0-9]{6}")
    assign(paste0("prob", semilla),
           fread(f))
    if (which(archivos == f) == 1) {
      ensemble <- get(paste0("prob", semilla))
      colnames(ensemble) <- c("numero_de_cliente",
                              "foto_mes",
                               paste0("p", semilla))
      ensemble[, paste0("rank", semilla) := frankv(ensemble,
                                                   cols = paste0("p", semilla))]
    } else {
      ensemble <- ensemble[get(paste0("prob", semilla)),
                                             on = "numero_de_cliente",
                                             paste0("p", semilla) := prob]
      ensemble[, paste0("rank", semilla) := frankv(ensemble,
                                                   cols = paste0("p", semilla))]
    }
  }
  col_rank = grep("^rank", colnames(ensemble))
  ensemble[ ,Predicted := rowMeans(.SD), .SDcols=col_rank]
  ensemble[rta ,clase_ternaria := clase_ternaria, on="numero_de_cliente"]
  return(ensemble)
}

##### FUncion que calcula ganancia #####
estimateGanancia <- function(ensemble) {
  col_p <- grep("^p_", colnames(ensemble), value=TRUE)
  col_p <- c(col_p, "Predicted")
  resultados <- data.table(envios = c(1:nrow(ensemble)))
  lista_optimos <- list()
  for (cl in col_p) {
    setorderv(ensemble, cols =cl, order= -1L)
    ensemble[ , ganancia := ifelse( clase_ternaria == "BAJA+2", 78000, -2000)]
    ensemble[, acumulado := cumsum(ganancia)]
    ensemble[, envios:=1:nrow(ensemble)]
    resultados <- resultados[ensemble, paste0(cl, "_ganancia") := acumulado, on="envios"]
    lista_optimos$envios <- append(lista_optimos$envios, 
                                   which.max(ensemble$acumulado))
    lista_optimos$maximo <- append(lista_optimos$maximo,
                                   (max(ensemble$acumulado)))
  }
  return_list <- list(df = resultados, lista = lista_optimos)
  return(return_list)
}


##### Armo ensambles y calculo ganancias #####
ensemble_original <- makeEnsemble(archivos_original, rta)
ganancia_original <- estimateGanancia(ensemble_original)

ensemble_no_drifting <- makeEnsemble(archivos_no_drifting, rta)
ganancia_no_drifting <- estimateGanancia(ensemble_no_drifting)

# ensemble_FE <- makeEnsemble(archivos_FE, rta)
# ganancia_FE <- estimateGanancia(ensemble_FE)

ensemble_ventana6 <- makeEnsemble(archivos_ventana6, rta)
ganancia_ventana6 <- estimateGanancia(ensemble_ventana6)

ensemble_no_2019 <- makeEnsemble(archivos_no_2019, rta)
ganancia_no_2019 <- estimateGanancia(ensemble_no_2019)

ensemble_trece_meses <- makeEnsemble(archivos_trece_meses, rta)
ganancia_trece_meses <- estimateGanancia(ensemble_trece_meses)

##### Armo tabla con ganancias maximas #####

##### Calculo la ganancia maz para cada experimentos #####
l_gm <- list(ganancia_original[["lista"]][["maximo"]],
          ganancia_no_drifting[["lista"]][["maximo"]],
          # ganancia_FE[["lista"]][["maximo"]],
          ganancia_ventana6[["lista"]][["maximo"]],
          ganancia_no_2019[["lista"]][["maximo"]],
          ganancia_trece_meses[["lista"]][["maximo"]])

l1 <- lengths(l_gm)
l2 <- c(l1[1]-1, 1,
        l1[2]-1, 1,
        # l1[3]-1, 1,
        l1[3]-1, 1,
        l1[4]-1, 1,
        l1[5]-1, 1)

ganancias_maximas <- data.table( experimento = rep(c("original", "no_drifting",
                                                    "ventana6", "no_2019",
                                                    "trece_meses"), times=l1),
                                ganancia = c(ganancia_original[["lista"]][["maximo"]],
                                              ganancia_no_drifting[["lista"]][["maximo"]],
                                              # ganancia_FE[["lista"]][["maximo"]],
                                              ganancia_ventana6[["lista"]][["maximo"]],
                                              ganancia_no_2019[["lista"]][["maximo"]],
                                              ganancia_trece_meses[["lista"]][["maximo"]]),
                                semillero = rep(c(0,1,0,1,0,1,0,1,0,1), 
                                                times = l2))
ganancias_maximas$semillero <- as.factor(ganancias_maximas$semillero)

##### plot maximo #####
level_order <- c("original", "no_drifting", "ventana6", "no_2019", "trece_meses")

ggplot(ganancias_maximas, aes(x=factor(experimento, level = level_order), y=ganancia, fill=factor(semillero)))+
  geom_dotplot(binaxis='y', stackdir='center') +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20)) 

##### plot curvas semillero ####

tmp_1 <- ganancia_original[["df"]][, c("envios", "Predicted_ganancia")]
tmp_2 <- ganancia_no_drifting[["df"]][, c("envios", "Predicted_ganancia")]
# tmp_3 <- ganancia_FE[["df"]][, c("envios", "Predicted_ganancia")]
tmp_4 <- ganancia_ventana6[["df"]][, c("envios", "Predicted_ganancia")]
tmp_5 <- ganancia_no_2019[["df"]][, c("envios", "Predicted_ganancia")]
tmp_6 <- ganancia_trece_meses[["df"]][, c("envios", "Predicted_ganancia")]

tmp <- tmp_1
tmp <- tmp[tmp_2, Predicted_ganancia_no_drifting := i.Predicted_ganancia, on="envios"]
# tmp <- tmp[tmp_3, Predicted_ganancia_FE := i.Predicted_ganancia, on="envios"]
tmp <- tmp[tmp_4, Predicted_ganancia_ventana6 := i.Predicted_ganancia, on="envios"]
tmp <- tmp[tmp_5, Predicted_ganancia_no_2019 := i.Predicted_ganancia, on="envios"]
tmp <- tmp[tmp_6, Predicted_ganancia_trece_meses := i.Predicted_ganancia, on="envios"]

ganancias_semilleros <- melt(tmp, id.vars="envios")

# Everything on the same plot
ggplot(ganancias_semilleros, aes(envios,value, color=variable)) + 
  xlim(7500, 15000) +
  ylim(4.0e7, 5.1e7) +
  geom_line()  +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.position="bottom") 








