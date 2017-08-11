######
###
# questo script è per capire meglio come viene fatto il grafico di un'analisi discriminante.


# NELLA LDA  i punteggi originali sono espressi come combinazioni lineari, ottimali per distinguere tra i gruppi.
## carico la history di analisi dati ICT, nella quale ci sono i dati che utilizzo per questa prova.
source('~/Desktop/Lavori unipd/ICT single trial analisi/history analisi dati ICT.R', chdir = TRUE)

## creo la matrice con le variabili che mi interessano
dati=scale(datall.sel[,c("RT", "ACC.sd", "Ampl", "MeanLatency", "RT.sd", "Ampl.sd","Latency.sd")])
dati.mat=as.matrix(dati)

# recupero i coefficienti della funzione discriminante
cols1=matrix(datall.lda$scaling[,1], ncol=1)
cols2=matrix(datall.lda$scaling[,2], ncol=1)

# tramite il prodotto matriciale che segue, creo la combinazione lineare delle colonne di "dati", secondo i pesi dati dai coefficienti delle funzioni discriminanti ld1 ed ld2.
# NOTA che scalo i risultandi, CENTRANDOLI (quindi media 0), ma non standardizzandoli.
prova.ld1=scale(prova.mat%*%cols1, scale=F, center=T)
prova.ld2=scale(prova.mat%*%cols2, scale=F, center=T)


provax=prova.ld1[datall.sel$group=="cirrotici_NOMHE",]
provay=prova.ld2[datall.sel$group=="cirrotici_NOMHE",]

## plotto il grafico automatico dell'analisi discriminante e lo confronto con le coordinate calcolate da me.
plot(datall.lda)
points(provax, provay, pch=19, col="blue")
