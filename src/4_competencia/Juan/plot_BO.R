#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require(data.table)
require(ggplot2)

setwd("D:/FCEN/Materias/DataMining/Materias/DMEF/labo/src/4_competencia/Juan/")

base <- fread("BO_log_base.txt")
FE <- fread("BO_log_FE.txt")
RF <- fread("BO_log_RF.txt")
full <- fread("BO_log_full.txt")


all <- list(base = base, FE = FE, RF = RF, full = full)
ganancias_BO <- rbindlist(all, use.names=TRUE, fill=FALSE, idcol=TRUE)

ggplot(ganancias_BO, aes(x = .id, y = ganancia)) +
  geom_dotplot(binaxis='y', stackdir='center')  +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(face="bold", color="#993333", 
                                   size=14, angle=45))
