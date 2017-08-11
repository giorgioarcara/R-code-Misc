##################################################
##FUNZIONE PER SCATTERPLOT CON COLORI DIVERSI#####
##################################################
## dep1 = il nome (tra virgolette) della prima variabile
## dep2 = il nome (tra virgolette) della seconda variabile
## fac = il nome (tra virgolette) della variabile fattore
## dataset = il nome del dataframe da cui ricavare le variabili

scatter=function(dep1, dep2, fac, dataset){
old.par=par()
dat=dataset
color.list=c("red", "blue", "gray", "black", "green", "magenta")
if (length(levels(dataset[,fac]))>length(color.list)){
	cat("Il numero di fattori è troppo alto, il grafico verrebbe una merda!!!!")
	}
else {
	ndep1=match(dep1,names(dat))
	ndep2=match(dep2,names(dat))
	nfac=match(fac,names(dat))
	par(mfrow=c(1,2), pty="s")
	for (i in 1:length(levels(as.factor(as.character(dataset[,nfac]))))) {
		if (i==1){
		plot(dat[dat[,nfac]==paste(levels(as.factor(as.character(dataset[,nfac])))[i]),ndep1], 	dat[dat[,nfac]==paste(levels(as.factor(as.character(dataset[,nfac])))[i]),ndep2], col=paste(color.list)[i], 	main=paste(toupper(names(dataset)[ndep1]),"X", toupper(names(dataset)[ndep2])), xlab=paste(names(dataset)[ndep1]), 	ylab=paste(names(dataset)[ndep2]), xlim=range(na.omit(dataset[,ndep1])),ylim=range(na.omit(dataset[,ndep2])), pty="s")
	par(new=TRUE)
	}
	if (i!=1){
	plot(dat[dat[,nfac]==paste(levels(as.factor(as.character(dataset[,nfac])))[i]),ndep1], 	dat[dat[,nfac]==paste(levels(as.factor(as.character(dataset[,nfac])))[i]),ndep2], col=paste(color.list)	[i],frame.plot=FALSE, axes=FALSE,main="", xlab="", ylab="", pty="s")
	par(new=TRUE)
				}
	}
	par(new=FALSE)
	plot(0,frame.plot=FALSE, axes=FALSE,main="", xlab="", ylab="", pty="s", type="n")
	legend("center",legend=levels(dat[,nfac]), col=color.list[1:i], pch="o")
 	}
par=old.par}