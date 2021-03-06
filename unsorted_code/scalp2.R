pscalp=function(categ, smo=0.5, label=c("type1"), layout=1, ylims="auto", yrev=TRUE, startmsec=-200, endmsec=1200, lwd=1, lty=1){

if (length(lwd)==1){
	lwd=rep(lwd, length(categ))}
if (length(lty)==1){
	lty=rep(lty, length(categ))}

if (class(categ)!="list"){
		stop("input object must be a list!!")}
if (layout==1){
electrodes=c("axes","Fp1", "blank", "Fp2","legend", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", "FC4", "FT8", "T3", "C3", "CZ","C4","T4","TP7", "CP3", "CPZ", "CP4", "TP8", "T5", "P3", "PZ", "P4", "T6", "blank", "O1", "OZ", "O2", "blank")
	}
	if (layout==2){
	electrodes=c("axes","Fp1", "FPZ", "Fp2","legend", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", "FC4", "FT8", "T7", "C3", "CZ","C4","T8","TP7", "CP3", "CPZ", "CP4", "TP8", "P7", "P3", "PZ", "P4", "P8", "blank", "O1", "OZ", "O2", "blank")
	}
		if (layout==3){
	electrodes=c("axes","Fp1", "Fpz", "Fp2","legend", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCz", "FC4", "FT8", "T3", "C3", "Cz","C4","T4","TP7", "CP3", "CPz", "CP4", "TP8", "T5", "P3", "PZ", "P4", "T6", "blank", "O1", "blank", "O2", "blank")
	}
	if (layout==4){
		electrodes=c("axes", "Fp1", "blank", "Fp2", "legend","blank", "AF3", "blank", "AF4", "blank", "F7", "F3", "Fz", "F4", "F8", "FC5", "FC1", "FCz", "FC2", "FC6", "T7", "C3", "Cz", "C4", "T8", "blank", "CP1", "CPz", "CP2", "blank", "P7", "P3", "Pz", "P4", "P8")
		}
	if (layout==5){
		electrodes=c("axes", "Fp1", "Fpz", "Fp2", "legend","blank", "AF3", "blank", "AF4", "blank", "F7", "F3", "Fz", "F4", "F8", "FC5", "FC1", "blank", "FC2", "FC6", "T7", "C3", "Cz", "C4", "T8", "CP5", "CP1", "blank", "CP2", "CP6", "P7", "P3", "Pz", "P4", "P8","PO7", "PO3", "POz", "PO4", "PO8", "blank","O1","Oz", "O2", "blank" )
		}		
		
color.list=c("blue","red","darkgreen")
## ci sono incongruenze con le etichette degli elettrodi. Per non fermarmi le cambio momentaneamente nella seguente #maniera T7=T3, T4=T8, P7=T5, T6=P8

if (ylims=="auto"){
	## mergio tutti i dataset per riscalare gli assi rispetto a massimo e minimo 
	alldata=NULL
		for (i in 1:length(categ)){
			alldata=rbind(alldata, categ[[i]])
		}
	ymax=max(alldata)
	ymin=min(alldata)
	yedge=max(c(ymax, abs(ymin)))#calcolo questo yedge in modo da fare limiti delle y simmetrici
	# aggiungo una perecentuale per evitare che il grafico sbordi (il)
	yedge=c(-yedge,yedge)
	}
if (ylims!="auto"){
	yedge=ylims
	yedge=c(-ylims, ylims)
	}	

if (yrev==TRUE){
	yedge=sort(yedge, decreasing=T)
	}

oldpar <- par(no.readonly=TRUE) #questo pezzo è per risettare alla fine della funzione i vecchi parametri. L'ho preso da "An introduction to R" pag. 68. Vedi anche sotto.
layout(matrix(1:35, 5, by=T))
par(mai=c(0,0,0,0))

if (layout==5)
   {
   par(mfrow=c(10,5), mai=c(0,0,0,0))
   }

plot(categ[[1]]$P4, type="n", frame.plot=FALSE,xlim=c(1,dim(categ[[1]])[1]),xaxt="n",yaxt="n", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)))
axis(side=2, pos= dim(categ[[1]])[1]/2, at=c(round(ceiling(yedge[1]),0),round(ceiling(yedge[1])/2,0),0,round(floor(yedge[2])/2,0),round(floor(yedge[2]),0)), cex.axis=0.8, las=2)
text((dim(categ[[1]])[1]/2)+(dim(categ[[1]])[1]/8),0, labels="μV", cex=1.4)
	for (i in 2:(length(electrodes))){
		if (electrodes[i]=="blank") {
			plot.new()
		}
		if (electrodes[i]=="legend"){
		plot.new()
		}
		if (electrodes[i]!="blank"&electrodes[i]!="axes"&electrodes[i]!="legend") {
			plot(smooth.spline(categ[[1]][[electrodes[i]]][1:dim(categ[[1]])[1]], spar=smo), type="l", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)),col=color.list[1], main="", ylab="", xlab="", cex.main=0.85,xlim=c(1,dim(categ[[1]])[1]),xaxt="n",yaxt="n",frame.plot=FALSE, lwd=lwd[1], lty=lty[1])
				##### di seguito ho semplicemente calcolato, tramite una proporzione, il punto che corrisponde allo 0
				totalendmsec=endmsec+abs(startmsec)
				zeropoint=(abs(startmsec)*dim(categ[[1]])[1])/totalendmsec
				segments(x0=zeropoint, y0=-0.8, x1=zeropoint, y1=0.5, lwd=1.5)
				abline(h=0, lty="longdash")
				mtext(electrodes[i],side=3, line=-2)
	if (length(categ)>1&electrodes[i]!="blank") {
				for (k in 2:length(categ)){
					lines(smooth.spline(categ[[k]][electrodes[i]], spar=smo),col=color.list[k], lwd=lwd[k],lty=lty[k])
					}
		} 
		}
	}
par(oldpar)#questo pezzo è per resettare alla fine della funzione i vecchi parametri. L'ho preso da "An 
#introduction to R" pag. 68. Vedi anche sotto.
print(color.list)
}
