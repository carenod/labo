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
  for (i in 1:10){
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := 10 ]  #registro en la planilla que tiran 70 tiros
  resultado1  <- gimnasio_tirar( ids_juegan1, 10)
  planilla_cazatalentos[ ids_juegan1,  paste0('aciertos', i) := resultado1 ]  #registro en la planilla
  }
  
  planilla_cazatalentos[ ids_juegan1,  mean := apply(planilla_cazatalentos[ids_juegan1,3:10],1,mean) ]
  
  planilla_cazatalentos[ ids_juegan1,  var := apply(planilla_cazatalentos[ids_juegan1,3:10],1,var) ]
  
  planilla_cazatalentos[ ids_juegan1,  cv := var / mean ]
  
  mediana  <- planilla_cazatalentos[ ids_juegan1, median(cv) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ cv > mediana, id ]
  veredicto <- sapply(gimnasio_veredicto(ids_juegan2)[2], sum)
  
  return(veredicto)
  
}
#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 102191 )  #debe ir una sola vez, ANTES de los experimentos

#tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )
tabla_veredictos  <- data.table(  outcome=integer())
lista_veredictos <- c()

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy
  
  veredicto  <- Estrategia_B()
  
  lista_veredictos  <- append( lista_veredictos, veredicto )
}
  