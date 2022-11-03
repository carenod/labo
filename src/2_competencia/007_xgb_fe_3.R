# mis semillas
# 298043, 133387, 987353, 135463, 287347

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#otros
require("Hmisc") #cut2

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

##### Defino hiperparametros y parametros #####

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
  makeNumericParam("learning_rate",    lower=    0.005, upper=    0.3),
  makeNumericParam("feature_fraction", lower=    0.2  , upper=    1.0),
  makeIntegerParam("min_data_in_leaf", lower=    0L   , upper=  8000L),
  makeIntegerParam("num_leaves",       lower=   16L   , upper=  1024L),
  makeIntegerParam("envios",           lower= 5000L   , upper= 15000L)
)

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM  <- list()

PARAM$experimento  <- "006_DAC2_menos_importantes_BO" # DAC porque es mio, 2 por la segunda competencia

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )

PARAM$trainingstrategy$undersampling  <-  1.0   # un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$semilla_azar   <- 298043  #Aqui poner la propia semilla

PARAM$hyperparametertuning$iteraciones <- 100
PARAM$hyperparametertuning$xval_folds  <- 5
PARAM$hyperparametertuning$POS_ganancia  <- 78000
PARAM$hyperparametertuning$NEG_ganancia  <- -2000

PARAM$hyperparametertuning$semilla_azar  <- 987353  #Aqui poner la propia semilla, PUEDE ser distinta a la de trainingstrategy

##### Defino funciones #####

#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}


#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_logistic_lightgbm  <- function( probs, datos) 
{
  vpesos   <- get_field(datos, "weight")
  
  #vector de ganancias
  vgan  <- ifelse( vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia, 
                   ifelse( vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia, 
                           PARAM$hyperparametertuning$NEG_ganancia / PARAM$trainingstrategy$undersampling ) )
  
  tbl  <- as.data.table( list( "vprobs" = probs, "vgan" = vgan ) )
  setorder( tbl,  -vprobs )
  ganancia <- tbl[ 1:GLOBAL_envios, sum( vgan ) ]
  
  return( list( "name"= "ganancia", 
                "value"=  ganancia,
                "higher_better"= TRUE ) )
}


#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria
  
  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1
  
  #para usar en fganancia_logistic_lightgbm 
  GLOBAL_envios <<- as.integer(x$envios/PARAM$hyperparametertuning$xval_folds)   #asigno la variable global
  
  kfolds  <- PARAM$hyperparametertuning$xval_folds   # cantidad de folds para cross validation
  
  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                          seed= PARAM$hyperparametertuning$semilla_azar
  )
  
  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )
  
  param_completo  <- c( param_basicos, param_variable, x )
  
  set.seed( PARAM$hyperparametertuning$semilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
  )
  
  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
  
  ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia
  
  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  
  #Voy registrando la importancia de variables
  if( ganancia_normalizada >  GLOBAL_gananciamax )
  {
    GLOBAL_gananciamax  <<- ganancia_normalizada
    modelo  <- lgb.train( data= dtrain,
                          param= param_completo,
                          verbose= -100
    )
    
    tb_importancia  <- as.data.table( lgb.importance(modelo ) )
    archivo_importancia  <- paste0( "impo_", GLOBAL_iteracion,".txt")
    fwrite( tb_importancia,
            file= archivo_importancia,
            sep= "\t" )
  }
  
  
  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra
  
  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )
  
  return( ganancia_normalizada )
}


##### Cargo datos #####

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory
# setwd("D:/FCEN/Materias/DataMining/Materias/DMEF")

#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread( PARAM$input$dataset )

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd( paste0( "./exp/", PARAM$experimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO

#en estos archivos quedan los resultados
kbayesiana  <- paste0( PARAM$experimento, ".RDATA" )
klog        <- paste0( PARAM$experimento, ".txt" )


GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_gananciamax <- -1 #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_gananciamax  <- tabla_log[ , max( ganancia ) ]
}

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes %in% PARAM$input$training, clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


##### Feature engineering #####
# Hago binnig 

# elijo variables para binnear
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

for( campo in  vars_to_bin )
{
  if(  dataset[ , length( unique( get(campo) ) ) > 100 ] )
  {
    dataset[  , paste0( campo, "_bin" ) := as.integer( cut2(  dataset[ , get(campo) ], m=1, g=31) ) ]
    dataset[  , paste0( campo ) := NULL ]
  }
  cat( campo, " " )
}


##### BO #####

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "azar", "training" ) )

set.seed( PARAM$trainingstrategy$semilla_azar )
dataset[  , azar := runif( nrow( dataset ) ) ]
dataset[  , training := 0L ]
dataset[ foto_mes %in% PARAM$input$training & 
           ( azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) ),
         training := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ training == 1L, campos_buenos, with=FALSE]),
                        label= dataset[ training == 1L, clase01 ],
                        weight=  dataset[ training == 1L, ifelse( clase_ternaria=="BAJA+2", 1.0000002, ifelse( clase_ternaria=="BAJA+1",  1.0000001, 1.0) )],
                        free_raw_data= FALSE  )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar, #la funcion que voy a maximizar
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,     #definido al comienzo del programa
  has.simple.signature = FALSE   #paso los parametros en una lista
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= PARAM$hyperparametertuning$iteraciones )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )

##### Para Kaggle post BO #####

setwd("D:/FCEN/Materias/DataMining/Materias/DMEF/")
PARAM <- list()
PARAM$experimento  <- "007_DAC2_FE_xgb_3"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.0054
PARAM$finalmodel$num_iterations    <-    1397
PARAM$finalmodel$num_leaves        <-   205
PARAM$finalmodel$min_data_in_leaf  <-   5717
PARAM$finalmodel$feature_fraction  <-      0.678664274618158
PARAM$finalmodel$semilla           <- 133387

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]


##### Data drifting #####

# los campos los decidí en
# 613_graficar_densidades_03_05_DAC.R
campos_frank <- scan(file = "./labo/src/2_competencia/campos_frank.txt",
                     what = character())

campos_malos <- scan(file = "./labo/src/2_competencia/campos_malos.txt",
                     what = character())

# Hago frank
for( campo in  campos_frank ) {
  dataset[ foto_mes == 202103, paste0(campo, "_r") := (frank(get(campo)) - 1) / (.N-1)]
  dataset[ foto_mes == 202105, paste0(campo, '_r') := (frank(get(campo)) - 1) / (.N-1)]
  
}

dataset[, c(campos_frank) := NULL]

dataset$ccajas_otras_r <- NULL
dataset$Visa_mpagado_r <- NULL
dataset$Visa_Finiciomora_r <- NULL

##### FE con xgboost #####
require("xgboost")

dataset[, clase_ternaria := NULL]

clase_binaria <- dataset[foto_mes == 202103, clase01]

dtrain <- xgb.DMatrix(
  data = data.matrix(dataset[foto_mes == 202103]),
  label = clase_binaria, missing = NA)

# Empecemos con algo muy básico
param_fe <- list(
  colsample_bynode = 0.8,
  learning_rate = 0.05,
  max_depth = 10, # <--- IMPORTANTE CAMBIAR
  num_parallel_tree = 10, # <--- IMPORTANTE CAMBIAR
  subsample = 0.8,
  objective = "binary:logistic"
)
nrounds <- 5

xgb_model <- xgb.train(params = param_fe, data = dtrain, nrounds = nrounds)

new_features <- xgb.create.features(model = xgb_model, data.matrix(dataset))
colnames(new_features)[150:173]

dataset1 <- data.matrix(new_features)
dataset1 <- as.data.frame(dataset1)
dataset1 <- as.data.table(dataset1)

##### lightGBM #####

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset1), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset1[ , train  := 0L ]
dataset1[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset1[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset1[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                      )
)

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset1[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 12000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  
  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )
