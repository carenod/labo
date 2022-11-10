
#grabo los datos donde voy a entrenar los Final Models
# aqui  JAMAS se hace sampling
fwrite( dataset[ foto_mes %in% PARAM$final_train, ],
        file= "dataset_train_final.csv.gz",
        logical01= TRUE,
        sep= "," )



#grabo los datos donde voy a hacer la optimizacion de hiperparametros
set.seed( PARAM$train$semilla_sampling )
dataset[ foto_mes %in% PARAM$train$training , azar_sampling := runif( nrow(dataset[foto_mes %in% PARAM$train$training ]) ) ]


set.seed( PARAM$train$semilla_under )
dataset[ foto_mes %in% PARAM$train$training , azar_under := runif( nrow(dataset[foto_mes %in% PARAM$train$training ]) ) ]

dataset[  , fold_train := 0L ]
dataset[ foto_mes %in% PARAM$train$training & 
           ( azar_sampling <= PARAM$train$sampling_total ) &
           ( azar_under <= PARAM$train$undersampling_mayoritaria | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) )
         , fold_train := 1L ]

#Se valida SIN sampling de ningun tipo
dataset[  , fold_validate := 0L ]
dataset[ foto_mes %in% PARAM$train$validation, fold_validate := 1L ]

#Se testea SIN sampling de ningun tipo
dataset[  , fold_test := 0L ]
dataset[ foto_mes %in% PARAM$train$testing, fold_test := 1L ]


fwrite( dataset[ fold_train + fold_validate + fold_test >= 1 , ],
        file= "dataset_training.csv.gz",
        logical01= TRUE,
        sep= "," )

