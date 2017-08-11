#INFLUENZA


# x = il vettore di dati su cui calcolare influenza di outliers
# perc = la percentuale (teorica) che deve delimitare il range
# ylim = il limite (in punti z)
 

infl=function(x, perc=0.9,	ylim=c(-3,3)){
	perc.ac=(1-perc)/2 #percentuale sulla quale vengono fatti veraemtne i calcoli
	edges=c(qnorm(perc.ac),-qnorm(perc.ac))
	mean.or=mean(x)
	infl=NULL
	for (i in 1:length(x)){
		mean.obs=mean(x[-i])
		infl[i]=mean.obs-mean.or
		}
	infl=scale(infl)
	#infl[infl<=ylim[1]]=ylim[1]
	#infl[infl>=ylim[2]]=ylim[2]
	kolmogorov=ks.test(x,pnorm)
	plot(infl,1:length(infl), xlim=ylim, pch=19, main=paste(perc*100,"%"," - Kolmogorov-Smirnov p = ",signif(kolmogorov$p.value,2)), col=!as.numeric((infl<edges[1]|infl>edges[2])))
	abline(h=0)
	lines(seq(-2,2,0.1),dnorm(seq(-2,2,0.1))*length(x))
	abline(v=edges, lty="dashed")
	infl.dat=as.data.frame(infl)
	infl.dat$out=infl<edges[1]|infl>edges[2]
	infl.dat[infl.dat[,1]<=ylim[1],1]=ylim[1]
	infl.dat[infl.dat[,1]>=ylim[2],1]=ylim[2]
	points(infl.dat[,1], 1:dim(infl.dat)[1], col=as.numeric(infl.dat$out==TRUE),, cex=3)
	text(infl.dat[,1], 1:dim(infl.dat)[1], labels=rownames(infl.dat), col=as.numeric(infl.dat$out==TRUE), cex=1.5)
	if (signif(kolmogorov$p.value,2)<0.05){
		mtext(side=3, "ATTENZIONE!!!! DISTRIBUZIONE NON NORMALE!")
		}
	return(infl)
	}