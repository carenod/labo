#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )
  
  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

Estrategia_B  <- function()
{
  #Estrategia
  #Se juegan varias rondas
  #En cada ronda, los jugadores que participan, tiran 70 tiros
  #De una ronda a la otra, solo pasan los que tuvieron igual o mayor aciertos a la mediana de aciertos de la ronda anterior
  #Se elige el mejor jugador de la sexta ronda
  
  gimnasio_init()
  
  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )
  
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   70  tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := 20 ]  #registro en la planilla que tiran 70 tiros
  resultado1  <- gimnasio_tirar( ids_juegan1, 20)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan1,  suma := aciertos1 ]
  
  #Ronda 2 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan1, median(suma) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ suma >= mediana, id ]
  
  planilla_cazatalentos[ ids_juegan2,  tiros2 := 20 ]  #registro en la planilla que tiran 70 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, 20)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan2,  suma := suma + aciertos2 ]
  
  #Ronda 3 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan2, median(suma) ]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ suma >= mediana, id ]
  
  planilla_cazatalentos[ ids_juegan3,  tiros3 := 20 ]  #registro en la planilla que tiran 70 tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, 20)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan3,  suma := suma + aciertos3 ]
  
  #Ronda 4 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan3, median(suma) ]
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][ suma >= mediana, id ]
  
  planilla_cazatalentos[ ids_juegan4,  tiros4 := 20 ]  #registro en la planilla que tiran 70 tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, 20)
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan4,  suma := suma + aciertos4 ]
  
  #Ronda 5 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan4, median(suma) ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][ suma >= mediana, id ]
  
  planilla_cazatalentos[ ids_juegan5,  tiros5 := 20 ]  #registro en la planilla que tiran 70 tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, 20)
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan5,  suma := suma + aciertos5 ]
  
  #Ronda 6 -------------------------------------------------------
  #A la mitad mejor la hago tirar 200 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan5, median(suma) ]
  ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][ suma >= mediana, id ]
  
  planilla_cazatalentos[ ids_juegan6,  tiros6 := 20 ]  #registro en la planilla que tiran 200 tiros
  resultado6  <- gimnasio_tirar( ids_juegan6, 20)
  planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan6,  suma := suma + aciertos6 ]
  
  #Ronda 7 -------------------------------------------------------
  #A la mitad mejor la hago tirar 1900 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  ids_juegan7  <- ids_juegan6
  
  planilla_cazatalentos[ ids_juegan7,  tiros7 := 400 ]  #registro en la planilla que tiran 200 tiros
  resultado7  <- gimnasio_tirar( ids_juegan7, 400)
  planilla_cazatalentos[ ids_juegan7,  aciertos7 := resultado7 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan7,  suma := suma + aciertos7 ]
  
  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(suma) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 102191 )  #debe ir una sola vez, ANTES de los experimentos

tabl
a_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy
  
  veredicto  <- Estrategia_B()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total 
tasa_eleccion_correcta

#Es una sábana corta ...


