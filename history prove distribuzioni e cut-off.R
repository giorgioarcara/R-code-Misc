
# simulo una distribuzione normale di punteggi ad un test neuropsi

set.seed(66)



###### PROVA 1 DISTRIBUZIONE NORMALE


anorm=rnorm(100, mean=20, sd=2)

par(mfrow=c(1,2))
# divido il device graphico in due perché voglio confrontare i due grafici

hist(anorm, main="distribuzione normale")

# calcolo il cut-off come paramatrico sulla base del valore che delimita 
# il 0.05 dei punteggi più bassi nella distribuzione con media dei miei punteggi e sd dei miei punteggi

cut.off_sd=qnorm(0.05, mean=mean(anorm), sd=sd(anorm))

# calcolo il cut-off come percentile non parametrico
cut.off_nonpar=quantile(anorm, prob=0.05)


abline(v=cut.off_sd, col="blue")

abline(v=cut.off_nonpar, col="red")

# nota che i valori sono molto simili




###### PROVA 2 DISTRIBUZIONE NORMALE CON EFFETTO CEILING

set.seed=20

anorm=rnorm(100, mean=20, sd=2)

# simulo un effetto ceiling mettendo a 20 tutti i valori di 20 o superiori

anorm[anorm>=20]=20

hist(anorm, main="distribuzione normale \n con effetto ceiling")

# calcolo il cut-off come paramatrico sulla base del valore che delimita 
# il 0.05 dei punteggi più bassi nella distribuzione con media dei miei punteggi e sd dei miei punteggi

cut.off_sd=qnorm(0.05, mean=mean(anorm), sd=sd(anorm))

# calcolo il cut-off come percentile non parametrico
cut.off_nonpar=quantile(anorm, prob=0.05)

abline(v=cut.off_sd, col="blue")

abline(v=cut.off_nonpar, col="red")

# nota che i valori si discostano di un po'.