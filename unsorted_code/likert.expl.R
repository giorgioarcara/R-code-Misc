#### LIKERT EXPLORE ######
# la funzione richiede gli items in colonna e i soggetti in riga. 


### !!!! FAI ANCHE FUNZIONE CHE CERCA (DATI I LIMITI DEI RANGE LIKERT), GLI ERRORI E TI DICE DOVE SONO! #####


# x = la matrice di dati, il nome di colonna di x è il nome di item
# subj.id = l'id del soggetti
# likert.range= il range di punteggi, utile per trovare errori
# groups = un vettore che indica l'appartenenza ad un gruppo.




likert.explore<-function(x, subj.id=rownames(x), likert.range=range(x, na.rm=T), cex.items=1, groups=NULL, tot.mean=TRUE){
	
	if(!(is.null(groups))){
		x=x[,names(x)[order(groups)]]	#reordering of Item names according to the vector group
	}
	
	x.mean=apply(x, 2, function(x){mean(x, na.rm=T)})
	x.sd=apply(x, 2, function(x){sd(x, na.rm=T)})
	
	if (tot.mean==T){
	x.tot.mean=mean(x.mean)
	names(x.tot.mean)="TOT"
	x.tot.sd=mean(x.sd)	
	names(x.tot.sd)="TOT"
	x.mean=c(x.mean, x.tot.mean)
	x.sd=c(x.sd, x.tot.sd)
	}
	
	n.items=ncol(x)
	
	pol.size=0.2
	
	x.min=likert.range[1]-1
	x.max=likert.range[2]+1
	
	plot(x.mean,1:length(x.mean), xlim=c(x.min, x.max), type="n", axes=F, xlab="Scores", ylab="", ylim=c(length(x.mean), 1))
	
	# create a grid
	
	rect(rep(x.min, n.items), (1:n.items)-pol.size, rep(x.max, n.items), (1:n.items)+pol.size, col=rep(c("grey95", "white"), n.items/2), border=rep(c("grey95", "white"), n.items/2))
	

	axis(side=2, at=1:length(x.mean), labels=names(x.mean), las=1, tick=F, cex.axis=cex.items)
	axis(side=1, at=seq(likert.range[1], likert.range[2], by=1))

	
	segments(x.mean, 1:length(x.mean)+pol.size, x.mean, 1:length(x.mean)-pol.size, lwd=2)
	
	segments(x.mean-x.sd, 1:length(x.mean)+pol.size, x.mean-x.sd, 1:length(x.mean)-pol.size,)
	segments(x.mean+x.sd, 1:length(x.mean)+pol.size, x.mean+x.sd, 1:length(x.mean)-pol.size)
	segments(x.mean-x.sd, 1:length(x.mean)+pol.size, x.mean+x.sd, 1:length(x.mean)+pol.size )
	segments(x.mean-x.sd, 1:length(x.mean)-pol.size, x.mean+x.sd, 1:length(x.mean)-pol.size )
	
	
	}