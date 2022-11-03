good_one <- c()
for( experimento  in  1:100 ) {
  good_one <- append(good_one, ftirar(0.7, 100))
}
good_one
hist(good_one)

bad_one <- c()
for( experimento  in  1:100 ) {
  bad_one <- append(bad_one, ftirar(0.55, 100))
}

hist(bad_one)


p1 <- hist(good_one)                     # centered at 4
p2 <- hist(bad_one)                     # centered at 6
plot( p1, col=rgb(0,0,1,1/4), xlim = c(20, 100))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


p1 <- hist(tabla_veredictos$tiros_total[tabla_veredictos$acierto == 1])                     # centered at 4
p2 <- hist(tabla_veredictos$tiros_total[tabla_veredictos$acierto == 0])                     # centered at 6
plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second
