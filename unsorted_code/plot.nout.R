## funzione per grafici dopo avere utilizzato ezANOVA.nout


#INPUT
# effects="all", quali effetti plottare, di default, TUTTI (altrimenti puoi specificare tu come testo)
# res.list = l'oggetto output di ezANOVA.nout, NOTA: come commento contiene il nome di variabili e interazioni.
# p.crit = DA COMPLETARE, il p.critico 
# plot.par=NULL si può specificare eventualmente il numero di righe e colonne del grafico di output
# cex.axis = 1


# VALUE
# una lista con due liste annidate: 
# - una lista di liste con due oggetti: 1) gli effetti risultati significativi nel test
									#	2) le unità statistiche tolte per quel test
# - una lista contenente le unità che tolte, danno origine ad effetti significativi (al momento senza distinguere per gli effetti)


plot.nout = function(effects="all", res.list, p.crit=0.05, plot.par=NULL, cex.x=1, plot.vars="all") {

	out=attr(res.list, "out")#devo recuperare l'oggetto con tutti i soggetti altrimenti il grafico mi mostra SOLO i soggetti per cui l'effetto significativo.


	
	if (length(effects)==1&effects[1]=="all"){
	alleffects=attr(res.list, "effects") } else {
		alleffects=effects
	}
	
	
	par(mfrow=c(1, length(alleffects)))
	
	
	if (!is.null(plot.par)){
		par(mfrow=c(plot.par))
	}
	
	if (plot.vars!="all"){
	alleffects=plot.vars
	}
	
	for (k in 1:length(alleffects)){
		eff=lapply(res.list, function(x) {grep(paste("^",alleffects[k],"$", sep=""), x[[2]]$Effect)})
		list.sig=res.list[!is.na(eff>1)] #recupero le sottoliste in cui è presente l'effetto. NOTA: dal momento che ho utilizzato ezresults, l'effetto è presente solo se significativo. Nota che però, per come è fatta la condizione logica if (any)
		list.sig.out=lapply(list.sig, function(x){ x[[1]]})
		sig.out=unlist(list.sig.out)
		table.sig.out=table(sig.out)
		add.names=levels(out)[!levels(out)%in%names(table.sig.out)]
		add.matrix=matrix(0, ncol=length(add.names), nrow=1)
		if (dim(add.matrix)[2]>0){
		final.matrix=cbind(add.matrix, t(matrix(table.sig.out))) #nota: il fatto che c'è t matrix è per una corretta combinazione di add.table e table.sig.out
		colnames(final.matrix)=c(add.names, names(table.sig.out))
		}
		if (dim(add.matrix)[2]==0){
		final.matrix=t(matrix(table.sig.out))
		colnames(final.matrix)=names(table.sig.out)
		}
		barplot(final.matrix, main=alleffects[k], col="gray", cex.names=cex.x)	
		}
}
