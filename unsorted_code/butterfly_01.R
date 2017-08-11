butterfly=function(dat,fac,group,electrode, main=NULL, smo=0.5 , col="black", startmsec=-200, endmsec=1000, yrev=TRUE, legend=FALSE, outline=NULL){
	if (col=="list"){
	col=c("black", "blue", "red", "darkgreen", "cyan", "darkblue", "blue", "brown", "azure", "orange", "lightblue", "orchid", "yellowgreen", "gray", "tan", "salmon")
		}
	else {
		col=rep(col,length(levels(fac)))
		}
	datsplit=split(dat, fac)
	lengthe1=length(datsplit[[1]][[electrode]])
	e1=datsplit[[1]][[electrode]]
	maxe1=max(dat[,electrode])
	mine1=min(dat[,electrode])
	if (smo!=0){	
		e1=smooth.spline(e1, spar=smo)
		} #effettuo un po' di smoothing sul segnale
	if (smo==0){
		e1=e1
		}
	if (yrev==TRUE){
		plot(e1, type="l", ylim=sort(range(c(6,-6, maxe1, mine1)), decreasing=T), main=main, xaxt="n", xlim=c(1,lengthe1), ylab="",xlab="", col=col, lwd=(sum(outline==1)*3+1))
		}
	if (yrev==FALSE){
		plot(e1, type="l", ylim=sort(range(c(6,-6, maxe1, mine1))), col=col, main=main, xaxt="n", xlim=c(1,lengthe1), ylab="",xlab="", outline=NULL, lwd=(sum(outline==1)*3+1))
		}
	vet=seq(startmsec, endmsec, round(endmsec/(endmsec/abs(startmsec))))
	temp=function(a){
	x=((a-(startmsec))*lengthe1)/(endmsec+abs(startmsec))
	return(x)}
	temp0=temp(0)
	vet2=temp(vet)
	vet.names=paste(vet) # vet sarebbero le labels del nuovo asse
	axis(1,vet2, paste(vet), cex.axis=0.8)
	abline(h=0, lty="longdash")
	segments(temp0,-0.5,temp0,0.5, lty=1.5)
	for (factors in 2:length(datsplit)){
		if (smo!=0){	
		en=smooth.spline(datsplit[[factors]][[electrode]], spar=smo)
		} #effettuo un po' di smoothing sul segnale
	if (smo==0){
		en=datsplit[[factors]][[electrode]]
		}
		lines(en, col=col[factors], lwd=sum(outline==factors)*3+1)
		}
	if (legend==TRUE){
		legend.labels=paste("Subject", 1:length(levels(fac)))
		legend("topright",legend=legend.labels, col=col, pch=19, cex=0.8, bg="white")}
	if (length(outline)!=0){
		mtext(text=paste("outlined Subjects:",paste(paste(outline), collapse="  ")),side=3)
	for (i in 1:length(outline)){
		cat("Subject",outline[i],":", levels(fac)[i], "\n")
		}
		}	
}
