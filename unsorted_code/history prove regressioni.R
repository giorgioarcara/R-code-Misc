### PROVE REGRESSIONI ###

load("/Users/giorgioarcara/Documents/Documenti/Dottorato/ENB2/analisi ENB2/enb2.RData")


enb2.tmta=na.omit(enb2[, c("age", "education", "tmta")])

tmta.lm=lm(tmta~age+education, enb2)

# per calcolare i  valori previsti moltiplico la matrice iniziale di dati (di age ed education)
# per un vettore colonna con i coefficienti. Nota che il vettore deve essere in colonna perché per fare
# una moltiplicazione tra matrice il numero di colonne della prima matrice (con gli observed data) deve 
# avere le stesse colonne delle righe della seconda matrice (che è un vettore con i coefficienti beta).

data.mat=(as.matrix(enb2.tmta[, c("age", "education")]))

# con questa riga aggiungo un vettore di 1 che mi serve per aggiungere l'intercetta alle stime.
data.mat=cbind(rep(1, dim(data.mat)[1]), data.mat)


beta.vec=(matrix(coef(tmta.lm), ncol=1))

pred.lm=  data.mat %*% beta.vec



pred.lm==predict(tmta.lm)



### PROVE ALGEBRA LINEARE

# genero una matrice quadrata

set.seed=100

A=matrix(rnorm(100), ncol=10)

# calcolo l'inversa di A

B=solve(A)

# nota che teoricamente A%*%B == B%*%A = I, con I = matrice identità
# in realtà i risultati sono leggermente diversi, dovuti a diverse approssimazioni numeriche.

A%*%B

B%*%A
