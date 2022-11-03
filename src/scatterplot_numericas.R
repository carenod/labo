##### plot de variables importantes #####

important_variables <- c('clase_binaria', names(modelo$variable.importance)[1:10])
dataset_iv  <- dataset[ foto_mes==202101 ]
dataset_iv <- dataset_iv[, ..important_variables]

for (i in 2:10) {
  ggplot(dataset_iv, 
         aes(x=important_variables[1], 
             y=important_variables[1+1],
             color = clase_binaria)) + 
    geom_point(alpha = 0.5)
}

alpha <- ifelse(dataset_iv$clase_binaria == 'NO', 0.01, 0.5)

ggplot(dataset_iv, aes(x=mcuentas_saldo, 
                       y=mcaja_ahorro, 
                       color = clase_binaria)) + 
  geom_point(alpha = alpha)

ggplot(dataset_iv, aes(x=mcuentas_saldo, 
                       y=mactivos_margen, 
                       color = clase_binaria)) + 
  geom_point(alpha = alpha)
