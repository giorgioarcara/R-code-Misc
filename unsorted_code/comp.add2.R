comp.add2=function(en, lty=1, smo=0.5, col="black", lwd=1, baseline=1, end=length(en)){
	en=smooth.spline(en, spar=smo) #effettuo un po' di smoothing sul segnale
	lines(baseline:end, en$y, col=col, lwd=lwd, lty=lty)}