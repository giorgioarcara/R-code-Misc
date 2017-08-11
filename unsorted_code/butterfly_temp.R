#nota: questa funzione è temporanea perchè presuppone certi nomi di gruppo e certe caratteristiche del dataset in input. L'ho creata solamente per fare le figure che servivano per il poster per Vancouver sulla prospective memory.
#Ad esempio nella riga 19 si assume che una colonna della lista datsplit, si chiami "monitor" e che questa variabile abbia i livelli "high" e "low".

butterfly=function(dat,fac,electrode,group, main=NULL, smo=0.5 , col="black", startmsec=-200, endmsec=1000, yrev=TRUE){
	datsplit=split(dat, fac)
	lengthe1=length(datsplit[[1]][[electrode]])
	e1=datsplit[[1]][electrode]
	maxe1=max(dat[,electrode])
	mine1=min(dat[,electrode])
	colorlist=c("violet", "darkgreen")
	if (smo!=0){	
		e1=smooth.spline(e1, spar=smo)
		} #effettuo un po' di smoothing sul segnale
	if (smo==0){
		e1=e1
		}
	if (yrev==TRUE){
		plot(e1, type="l", ylim=sort(range(c(6,-6, maxe1, mine1)), decreasing=T),lwd=1, main=main, xaxt="n", xlim=c(1,lengthe1), ylab="",xlab="", col=as.numeric(mean(datsplit[[1]]$monitor=="high"))*10+as.numeric(mean(datsplit[[1]]$monitor=="low")*20))
		}
	if (yrev==FALSE){
		plot(e1, type="l", ylim=sort(range(c(6,-6, maxe1, mine1))),lwd=1, main=main, xaxt="n", xlim=c(1,lengthe1), ylab="",xlab="",col=as.numeric(mean(datsplit[[1]]$monitor=="high"))*10+as.numeric(mean(datsplit[[1]]$monitor=="low")*20),cex.main=3, cex.axis=1.8)
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
	for (factors in 2:length(datsplit)){
		lines(datsplit[[factors]][[electrode]], col=as.numeric(mean(datsplit[[factors]]$monitor=="high"))*10+as.numeric(mean(datsplit[[factors]]$monitor=="low")*20))
		}
}
