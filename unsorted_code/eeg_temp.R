eeg=function(e1,main=NULL, smo=0.5 , col="black", startmsec=-200, endmsec=1000, yrev=TRUE){
	lengthe1=length(e1)
	maxe1=max(e1)
	mine1=min(e1)
	if (smo!=0){	
		e1=smooth.spline(e1, spar=smo)
		} #effettuo un po' di smoothing sul segnale
	if (smo==0){
		e1=e1
		}
	if (yrev==TRUE){
		plot(e1, type="l", ylim=sort(range(c(6,-6, maxe1, mine1)), decreasing=T), col=col,lwd=2, main=main, xaxt="n", xlim=c(1,lengthe1), ylab="",xlab="", cex.main=2.5, cex.lab=2, cex.axis=1.7)
		}
	if (yrev==FALSE){
		plot(e1, type="l", ylim=sort(range(c(6,-6, maxe1, mine1))), col=col,lwd=2, main=main, xaxt="n", xlim=c(1,lengthe1), ylab="",xlab="",cex.main=2.5, cex.lab=2, cex.axis=1.7)
		}
	vet=seq(startmsec, endmsec, round(endmsec/(endmsec/abs(startmsec))))
	temp=function(a){
	x=((a-(startmsec))*lengthe1)/(endmsec+abs(startmsec))
	return(x)}
	temp0=temp(0)
	vet2=temp(vet)
	vet.names=paste(vet) # vet sarebbero le labels del nuovo asse
	axis(1,vet2, paste(vet), cex.axis=1.7)
	abline(h=0, lty="longdash")
	segments(temp0,-0.5,temp0,0.5, lty=1.5)
}
