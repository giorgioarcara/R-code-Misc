## ezANOVA + jackknife per valutare stabilità degli effetti.
# la funzione permette di valutare la stabilità degli effetti di un anova togliendo n unità statistiche (es. soggetti) alla volta.

#INPUT
# ..., argomenti da ez ANOVA
# out = oggetto .() contennte il nome della variabile che verrà trattata dal jackknife
# n = il numero di unità da togliere
# p.crit = DA COMPLETARE, il p.critico 
# plot = logico, se fare un plot dei risultati. Le barre indicano se l'effetto è significativo togliendo il soggetto in questione
# plot.par=NULL si può specificare eventualmente il numero di righe e colonne del grafico di output
# cex.axis = 1
# plot.vars = le variabili da plottare, come default tutte
# details= F

# VALUE
# una lista con due liste annidate: 
# - una lista di liste con due oggetti: 1) gli effetti risultati significativi nel test
									#	2) le unità statistiche tolte per quel test
# - una lista contenente le unità che tolte, danno origine ad effetti significativi (al momento senza distinguere per gli effetti)

# NOTA!!! la funzione sembra non funzionare più perché è cambiato il comportamento di .() legato a ... nell'argomento della funzione.


ezANOVA.1out = function(vet1=NULL, vet2=NULL, paired=F, var.equal=T, out, p.crit=0.05, plot=FALSE, plot.par=NULL, cex.x=1, plot.vars="all", ...) {
	
	
		
	out.l=length(vet1)+length(vet2)
	#out.s=combn(out.l, out.l-n) devi lavorare qui per fare nout e non 1 out. Solo che il problema è che devi creare prima devi creare un data.frame
	cat("  ", out.l, "possibile combinations will be evaluated\n")
	counter=0
	for (i in 1:out.l){
	
	if (i <= length(vet1)){
		newvet1=vet1[-i]
		cat("current subject removed: ", i, " of vet 1\n")
		} else {
		
		newvet1=vet1
		newvet2=vet2[-(i-length(vet2)+1)]
		
		cat("current subject removed: ",(i-length(vet2)+1), "of vet 2\n")
		
		}
		
		t.test(newvet1, vet2)	
		
		#####
				
			if(any(res.sig$p<p.crit)){
				counter=counter+1
				if (counter==1){
					res.list=list(list(out=levels(out)[!levels(out)%in%out.s[,i]],res.sig))
				}
				if (counter>1){
					res.list[[counter]]=list(levels(out)[!levels(out)%in%out.s[,i]],res.sig)
				}
			}
		
	}
	alleffects=res$ANOVA$Effect
	attr(res.list, "effects")=alleffects
	attr(res.list, "out")=out

	#if (plot==TRUE){
	
	#if (plot.vars!="all"){
	#alleffects=plot.vars
	#}
	
	
	#if (!is.null(plot.par)){
	#	par(mfrow=c(plot.par))} else {
	#		par(mfrow=c(1,length(alleffects)))
	#		}

	
	#for (k in 1:length(alleffects)){
	#	eff=lapply(res.list, function(x) {grep(paste("^",alleffects[k],"$", sep=""), x[[2]]$Effect)})
	#	list.sig=res.list[!is.na(eff>1)] #recupero le sottoliste in cui è presente l'effetto. NOTA: dal momento che ho utilizzato ezresults, l'effetto è presente solo se significativo. Nota che però, per come è fatta la condizione logica if (any)
	#	list.sig.out=lapply(list.sig, function(x){ x[[1]]})
	#	sig.out=unlist(list.sig.out)
	#	table.sig.out=table(sig.out)
	#	add.names=levels(out)[!levels(out)%in%names(table.sig.out)]
	#	add.matrix=matrix(0, ncol=length(add.names), nrow=1)
	#	if (dim(add.matrix)[2]>0){
	#	final.matrix=cbind(add.matrix, t(matrix(table.sig.out))) #nota: il fatto che c'è t matrix è per una corretta combinazione di add.table e table.sig.out
	#	colnames(final.matrix)=c(add.names, names(table.sig.out))
	#	}
	#	if (dim(add.matrix)[2]==0){
	#
	#	final.matrix=t(matrix(table.sig.out))
	#	colnames(final.matrix)=names(table.sig.out)
	#	}
	#	barplot(final.matrix, main=alleffects[k], col="gray", cex.names=cex.x)	
	#	}
	}

 return(res.list)#, matrix(unlist(list.sig.out), ncol=n,byrow=TRUE)))
 
}
