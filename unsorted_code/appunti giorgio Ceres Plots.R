### HISTORY APPUNTI CERES PLOT #####

#####
setwd("/Users/giorgioarcara/Desktop/NADL")
source("/Users/giorgioarcara/Desktop/R files/summarytab.R")

load("nadl.RData")
library(car)

prova=lm(formal.tot~age+education, nadl)

## calcolo i predicted parziali, cioè i predicted usando solo in parte i coefficienti del modello completo.
partialpred=partialpred=61.36805+(0.86756*nadl$education)
partialres=nadl$formal.tot-partialpred

partialres=-partialpred+nadl$formal.tot-18 ### il -18 è una costante che ho trovato empiricamente. 

plot(nadl$age, partialres)

ceresPlots(prova, terms=~age)
points(nadl$age, partialres, pch="x")


#nota che i due grafici hanno forma identica, ma il grafico fatto con plot, è shiftato di una costante 
# (non sono riuscito a capire da dove è presa e non sono riuscito a trovare la formula del ceresPlot su internet)