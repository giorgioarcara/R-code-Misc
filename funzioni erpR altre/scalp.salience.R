scalp.salience <-
function(salience, bootstrap, smo=0.5, label=c("type1"), layout=1, ylims="auto", yrev=TRUE, startmsec=-200, endmsec=1200, lwd=1, lty=1, color.list=c("blue", "red", "darkgreen"), legend=F, legend.lab="default", t.axis=seq(-100,endmsec,200), boot.col="blue", boot.pch=23, boot.bg="blue",boot.pos=6,boot.cex=1){

###### INSERISCO LA FUNZIONE PLOT BOOT
plot.boot=function(x, pch=23, bg="blue", col="blue", pos=6, cex=1){
	points(grep(1,x), rep(pos,length(grep(1,x))), pch=pch, bg=bg, col=col, cex=cex)
}

if (length(legend.lab)==1&legend.lab[1]=="default"){
legend.lab=deparse(substitute(salience))
legend.lab=gsub("\\(", "",legend.lab)
legend.lab=gsub("\\)", "", legend.lab)
legend.lab=gsub("^list", "", legend.lab)
legend.lab=gsub(" ", "", legend.lab)
legend.lab=unlist(strsplit(legend.lab, ","))
}

msectopoints=function(a,lengthsegment){
	x=((a-(startmsec))*lengthsegment)/(endmsec+abs(startmsec))
	return(x)}
				

if (length(lwd)==1){
	lwd=rep(lwd, length(salience))}
if (length(lty)==1){
	lty=rep(lty, length(salience))}

if (class(salience)!="list"){
		stop("input object must be a list!!")}
if (layout==1){
electrodes=c("axes","Fp1", "blank", "Fp2","legend", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", "FC4", "FT8", "T3", "C3", "CZ","C4","T4","TP7", "CP3", "CPZ", "CP4", "TP8", "T5", "P3", "PZ", "P4", "T6", "axis", "O1", "OZ", "O2", "blank")
	}
	if (layout==2){
	electrodes=c("axes","Fp1", "FPZ", "Fp2","legend", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", "FC4", "FT8", "T7", "C3", "CZ","C4","T8","TP7", "CP3", "CPZ", "CP4", "TP8", "P7", "P3", "PZ", "P4", "P8", "axis", "O1", "OZ", "O2", "blank")
	}
		if (layout==3){
	electrodes=c("axes","Fp1", "Fpz", "Fp2","legend", "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCz", "FC4", "FT8", "T3", "C3", "Cz","C4","T4","TP7", "CP3", "CPz", "CP4", "TP8", "T5", "P3", "PZ", "P4", "T6", "axis", "O1", "blank", "O2", "blank")
	}
		if (layout==4){
		electrodes=c("axes", "Fp1", "blank", "Fp2", "legend","blank", "AF3", "blank", "AF4", "blank", "F7", "F3", "Fz", "F4", "F8", "FC5", "FC1", "FCz", "FC2", "FC6", "T7", "C3", "Cz", "C4", "T8", "blank", "CP1", "CPz", "CP2", "blank", "P7", "P3", "Pz", "P4", "P8", "blank","O1","blank", "O2", "blank")
		}
	if (layout==5){
		electrodes=c("axes", "Fp1", "Fpz", "Fp2", "legend","blank", "AF3", "blank", "AF4", "blank", "F7", "F3", "Fz", "F4", "F8", "FC5", "FC1", "blank", "FC2", "FC6", "T7", "C3", "Cz", "C4", "T8", "CP5", "CP1", "blank", "CP2", "CP6", "P7", "P3", "Pz", "P4", "P8","PO7", "PO3", "POz", "PO4", "PO8", "blank","O1","Oz", "O2", "blank" )
		}
		if (layout==6){
		electrodes=c("axes","Fp1", "Fpz", "Fp2","legend", "F7", "F3", "Fz", "F4", "F8", "Ft7", "Fc3", "Fcz", "Fc4", "Ft8", "T3", "C3", "Cz","C4","T4","Tp7", "Cp3", "Cpz", "Cp4", "Tp8", "T5", "P3", "Pz", "P4", "T6", "axis", "O1", "blank", "O2", "blank")
		}			
		
## ci sono incongruenze con le etichette degli elettrodi. Per non fermarmi le cambio momentaneamente nella seguente #maniera T7=T3, T4=T8, P7=T5, T6=P8

if (ylims=="auto"){
	## mergio tutti i dataset per riscalare gli assi rispetto a massimo e minimo 
	alldata=NULL
		for (i in 1:length(salience)){
			alldata=rbind(alldata, salience[[i]])
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

oldpar <- par(no.readonly=TRUE) #questo pezzo <U+00E8> per risettare alla fine della funzione i vecchi parametri. L'ho preso da "An introduction to R" pag. 68. Vedi anche sotto.
par(mfrow=c(7,5), mai=c(0,0,0,0))
if (layout==5)
   {
   par(mfrow=c(10,5), mai=c(0,0,0,0))
   }
if (layout==4)
   {
   par(mfrow=c(8,5), mai=c(0,0,0,0))
   }

plot(salience[[1]]$P4, type="n", frame.plot=FALSE,xlim=c(1,dim(salience[[1]])[1]),xaxt="n",yaxt="n", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)))
axis(side=2, pos=dim(salience[[1]])[1]/2, at=c(round(ceiling(yedge[1]),0),round(ceiling(yedge[1])/2,0),0,round(floor(yedge[2])/2,0),round(floor(yedge[2]),0)), cex.axis=0.8, las=2)
text((dim(salience[[1]])[1]/2)+(dim(salience[[1]])[1]/8),0, labels=expression(paste(mu,"V")), cex=1.4)
	for (i in 2:(length(electrodes))){
		if (electrodes[i]=="blank") {
			plot.new()
		}
		if (electrodes[i]=="legend"){
			plot.new()
			if (legend=="TRUE"){
	legend("center", legend=legend.lab, col=color.list, cex=1.2, lty=lty, lwd=lwd) #pch=15, pt.bg=color.list
			}
		}
		if (electrodes[i]=="axis"){
plot(salience[[1]]$P4, type="n", frame.plot=FALSE,xlim=c(1,dim(salience[[1]])[1]),xaxt="n",yaxt="n", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)))

			axis(1, pos=c(msectopoints(startmsec,dim(salience[[1]])[1]),0), at=msectopoints(t.axis, dim(salience[[1]])[1]), labels=paste(t.axis))
		}
		if (electrodes[i]!="blank"&electrodes[i]!="axis"&electrodes[i]!="legend") {
			plot(smooth.spline(salience[[1]][[electrodes[i]]][1:dim(salience[[1]])[1]], spar=smo), type="l", ylim=c(yedge[1]+yedge[1]/3,yedge[2]+(yedge[2]/3)),col=color.list[1], main="", ylab="", xlab="", cex.main=0.85,xlim=c(1,dim(salience[[1]])[1]),xaxt="n",yaxt="n",frame.plot=FALSE, lwd=lwd[1], lty=lty[1])				
				##### di seguito ho semplicemente calcolato, tramite una proporzione, il punto che corrisponde allo 0

				totalendmsec=endmsec+abs(startmsec)
				zeropoint=msectopoints(0, dim(salience[[1]])[1])
				
				segments(x0=zeropoint, y0=-0.8, x1=zeropoint, y1=0.5, lwd=1.5)
				abline(h=0, lty="longdash")
				mtext(electrodes[i],side=3, line=-2)
			
			plot.boot(bootstrap[[electrodes[i]]],col=boot.col, pch=boot.pch, bg=boot.bg, pos=boot.pos ,cex=boot.cex) 
		}
	}
par(oldpar)#questo pezzo <U+00E8> per resettare alla fine della funzione i vecchi parametri. L'ho preso da "An 
#introduction to R" pag. 68. Vedi anche sotto.
}

