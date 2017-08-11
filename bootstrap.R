#### BOOTSTRAP ######
## calcola intervalli di confidenza di effetti fissi ed effetti random di un modello mer tramite bootstrap.
# la parte sui fattori fissi è prasa ad Baayen (2007) Analyzing Linguistic Data, pag. 283
# la parte su fattori random è scritta da me.
# ho provato a confrontare con la funzione intervals del pacchetto nlme fittando un modello mer sull'esempio di pag 25 di Pinheiro e Bates. I risultati che ho ottenuto sono però diversi. Rispetto alla prova sembra che l'intervallo calcolato tramite bootstrap sia più o meno della stessa ampiezza, ma spostato leggermente più in basso (quindi il lower più basso e l'upper più basso). 

### NOTA!!! non funziona se dai delle trasformazioni direttamente nella formula es. log(RT)~type. In questo caso ricrea prima una variabile es. log_RT~type.


# INPUT
#-dat.lmer= il modello mer fittato
#-sid= l'identificativo dei Soggetti.
#-nruns= il numero di campioni estratti

bootstrap=function(dat.lmer, sid, nruns=100){
#input per poi fare funzione

formula.lmer=formula(dat.lmer)
dataframe=dat.lmer@frame #estraggo il dataframe dall'oggetto mer
Subjects=factor(sid)
Subjectslev=levels(Subjects)

perc=100 ### DA CONTINUARE!!!!! all'interno di un soggetto prendi solo parte dei dati?
#


for (run in 1:nruns){
	### campiono con rimpiazzamento dei Soggetti
	mysampleofSubjects=sample(Subjectslev, replace=TRUE)
	# seleziona righe dal dataframe dei soggetti campionati
	mysample=dataframe[is.element(Subjects, mysampleofSubjects),]
	mysample.lmer=lmer(formula.lmer, data=mysample)
	fixedEffects=fixef(mysample.lmer)
	randomEffects.tot=VarCorr(mysample.lmer)
	randomEffects=NULL
	tvals=as.data.frame(summary(mysample.lmer)@coefs[,"t value"])
	tvals=t(tvals)
	for (nranef in 1:length(randomEffects.tot))
	{
	randomEffects=cbind(randomEffects,randomEffects.tot[[nranef]][1])
	}
	if (run==1) {
	res=fixedEffects 
	resran=randomEffects
	rest=tvals
	}
	else {
	res=rbind(res, fixedEffects) 
	resran=rbind(resran, randomEffects)
	rest=rbind(rest, tvals)
	}
	cat(".")
	}
	cat("\n")
	rownames(res)=1:nruns
	res=data.frame(res)
	rownames(resran)=1:nruns
	resran=data.frame(resran)
	names(resran)=names(randomEffects.tot)#metto i nomi prendendo da
	## valori da ritornare
	res.mat=t(apply(res, 2, quantile, c(0.025,0.5, 0.975)))
	resran.mat=t(apply(resran,2,quantile, c(0.025, 0.5, 0.975)))
	rest.mat=t(apply(rest,2,quantile, c(0.025, 0.5, 0.975)))
	results=list(fixed=res.mat, random=resran.mat, t.values=rest)
	invisible(results)
	print(res.mat)
	print(resran.mat)
	print(rest.mat)
	}