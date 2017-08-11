###
#

# per ricavare i dati PLS dei design scores. In matlab prima carica il file con i risultati.
# load SAMI14sogg_no11_ERPresult.mat
# designlv(:,1) #sono i valori per la prima variabile latente (nota che sono per colonne)
# bar(designlv(:,1)) #per un barplot che dimostra che è uguale al grafico fatto da plsgui


#saliency è il vettore con le salienze
#bs è il vettore logico che indica in che punti il bootstrap è risultato significativo.

saliency=read.table("/Users/giorgioarcara/Documents/Documenti/Dottorato/Matrici SAMI/Dualtask/picture_ec1_14sogg.txt", header=TRUE)

source("/Users/giorgioarcara/Desktop/R files/eeg.R")

bs=rep(0, length(saliency$O2))
bs[40:100]=rep(1, length(bs[40:100]))
eeg(saliency$O2)
points(jitter(grep(1,bs),400),rep(6,length(grep(1,bs))), pch=23, bg="black")