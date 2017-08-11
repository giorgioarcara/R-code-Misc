# EFFECTS OF COLLINEARITY
# in queste linee di codice si vede come la collinearità tra predittori può influenzare una regressione multipla.

x=rnorm(100)
q=x+rnorm(100,sd=0.01)
y=x+rnorm(100)


#I predittori (x e q) sono entrambi fortemente correlati alla variabile indipendente y. Se li considero singolarmente in due regressioni semplici separate (modelx, model )ottengo circa un 45% di varianza spiegata.

modelx=lm(y~x)
modelq=lm(y~q)

#Se però li considero insieme, pur ottenendo la stessa quantità di varianza spiegata, nessuno dei due predittori ha una slope significativa.


modelxq=lm(y~x+q)

summary(modelxq)

# per risolvere questo problema potrei fare una analisi delle componeti principali sui due predittori.(sacle.=TRUE è per normalizzare le variabili prima delle prcomp)
xq.pc=prcomp(dati[,c("x","q")], scale.=TRUE) 
xq.props=round((xq.pc$sdev^2/sum(xq.pc$sdev^2)),5)#calcolo la percentuale di varianza riconducibile a ciascuna componente
# quasi il 100% della varianza è riconducibile ad una sola componente, che può essere utilizzata invece di tutti e due i predittori (che sono appunto altamente collineari).


