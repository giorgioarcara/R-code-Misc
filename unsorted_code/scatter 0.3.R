##################################################
##FUNZIONE PER SCATTERPLOT CON COLORI DIVERSI#####
##################################################

## ha problemi con i dati mancanti: attualmente ho risolto il problema sostituendo la funzione lowess con loess, ma il risultato non va bene, visto che le linee che vengono fuori sono troppo "spezzate" (si dovrebbero regolare i parametri)


## dep1 = il nome (tra virgolette) della prima variabile
## dep2 = il nome (tra virgolette) della seconda variabile
## fac = il nome (tra virgolette) della variabile fattore
## dat = il nome del dataframe da cui ricavare le variabili

scatter=function(dep1, dep2, fac, dat, pch=21, cex=1){
color.list=c("red", "blue", "gray", "black", "green", "magenta")
if (length(levels(factor(dat[,fac])))>length(color.list)){
	cat("Il numero di fattori è troppo alto, il grafico verrebbe una merda!!!!")
	}
else {
	dat=na.omit(dat[,c(dep1, dep2, fac)])
	ndep1=match(dep1,names(dat))
	ndep2=match(dep2,names(dat))
	nfac=match(fac,names(dat))
	for (i in 1:length(levels(as.factor(as.character(dat[,nfac]))))) {
		if (i==1){
		plot(dat[dat[,nfac]==paste(levels(as.factor(as.character(dat[,nfac])))[i]),ndep1], 	dat[dat[,nfac]==paste(levels(as.factor(as.character(dat[,nfac])))[i]),ndep2], col=paste(color.list)[i], 	main=paste(toupper(names(dat)[ndep1]),"X", toupper(names(dat)[ndep2])), xlab=paste(names(dat)[ndep1]), 	ylab=paste(names(dat)[ndep2]), xlim=range(na.omit(dat[,ndep1])),ylim=range(na.omit(dat[,ndep2])), pty="s",pch=pch,cex=cex,bg=paste(color.list)[i])
	lines(lowess(dat[dat[,nfac]==paste(levels(as.factor(as.character(dat[,nfac])))[i]),ndep1],	dat[dat[,nfac]==paste(levels(as.factor(as.character(dat[,nfac])))[i]),ndep2]), col=paste(color.list)[i])
	par(new=TRUE)
	}
	if (i!=1){
	plot(dat[dat[,nfac]==paste(levels(as.factor(as.character(dat[,nfac])))[i]),ndep1], 	dat[dat[,nfac]==paste(levels(as.factor(as.character(dat[,nfac])))[i]),ndep2], col=paste(color.list)	[i],frame.plot=FALSE, axes=FALSE,main="", xlab="", ylab="", pty="s", pch=pch,cex=cex,bg=paste(color.list)[i])
	lines(lowess(dat[dat[,nfac]==paste(levels(as.factor(as.character(dat[,nfac])))[i]),ndep1], 	dat[dat[,nfac]==paste(levels(as.factor(as.character(dat[,nfac])))[i]),ndep2]), col=paste(color.list)[i])
	par(new=TRUE)}
	}
	legend("topright",legend=levels(as.factor(as.character(dat[,nfac]))), col=color.list[1:i], pch=19, cex=0.8)
 	}
par(new=FALSE)
}