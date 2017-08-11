scalp=function(categ, smo=0.5, label=c("type1"), layout=1, ylims="auto", yrev=TRUE, basestart=-200, winlength=1200){
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
color.list=c("violetred", "darkgreen","black", "gray","red","blue","red", "green", "blue")
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

par(mfrow=c(7,5), mai=c(0,0,0,0))
plot(categ[[1]]$P4, type="n", frame.plot=FALSE,xlim=c(1,dim(categ[[1]])[1]),xaxt="n",yaxt="n", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)))
axis(side=2, pos= dim(categ[[1]])[1]/2, at=c(round(ceiling(yedge[1]),0),round(ceiling(yedge[1])/2,0),0,round(floor(yedge[2])/2,0),round(floor(yedge[2]),0)), cex.axis=1.7, las=2)
text((dim(categ[[1]])[1]/2)+(dim(categ[[1]])[1]/8),0, labels="μV", cex=2)
	for (i in 2:(length(electrodes))){
		if (electrodes[i]=="blank") {
			plot.new()
		}
		if (electrodes[i]=="legend"){
		plot.new()
		}
		if (electrodes[i]!="blank"&electrodes[i]!="axes"&electrodes[i]!="legend") {
			plot(smooth.spline(categ[[1]][[electrodes[i]]][1:dim(categ[[1]])[1]], spar=smo), type="l", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)),col=color.list[1], main="", ylab="", xlab="", cex.main=2,xlim=c(1,dim(categ[[1]])[1]),xaxt="n",yaxt="n",frame.plot=FALSE, lty=1.5)
				##### di seguito ho semplicemente calcolato, tramite una proporzione, il punto che corrisponde allo 0
				totalwinlength=winlength+abs(basestart)
				zeropoint=(abs(basestart)*dim(categ[[1]])[1])/totalwinlength
				segments(x0=zeropoint, y0=-0.8, x1=zeropoint, y1=0.5, lwd=1.5)
				abline(h=0, lty="longdash")
				mtext(electrodes[i],side=3, line=-2, cex=1.5)
	if (length(categ)>1&electrodes[i]!="blank") {
				for (k in 2:length(categ)){
					lines(smooth.spline(categ[[k]][electrodes[i]], spar=smo),col=color.list[k], lwd=1.5)
					}
		} 
		}
	}
par(oldpar)#questo pezzo è per resettare alla fine della funzione i vecchi parametri. L'ho preso da "An 
#introduction to R" pag. 68. Vedi anche sotto.
}
