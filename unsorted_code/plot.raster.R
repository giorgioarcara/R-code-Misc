plot.raster<-function(res, startmsec=NULL, endmsec=NULL, palette.col="jet", high.filt=NULL, lower.thresh=NULL, upper.thresh=NULL, zlim="default"){

if (all(is.na(unlist(res)))){
  stop("No significant results to plot", call.=F)
}
  	
res=as.matrix(res)

par(mar=c(5.1, 8.0, 4.1, 2.1))
plot(0,0, xlim=c(1, dim(res)[1]), ylim=c(1, dim(res)[2]), type="n", xlab="", ylab="", axes=F, frame.plot=F)
abline(v=1:dim(res)[1]-0.5, col="lightgray", lwd=0.3) # note the shift -0.5 to center the little rectangles.
abline(h=1:dim(res)[2]-0.5, col="lightgray", lwd=0.3)

if (zlim[1]=="default"){
  myzlim=range(res, na.rm=T)
} else {
  myzlim=zlim
}


if (!is.null(lower.thresh) & is.null(upper.thresh)){
  res[res < lower.thresh]=NA
}

if (!is.null(upper.thresh) & is.null(lower.thresh)){
  res[res<upper.thresh]=NA
}

if (!is.null(upper.thresh) & ! is.null(lower.thresh)){
  res[res < upper.thresh & res > lower.thresh]=NA
}



### GRAFICO

if (palette.col=="heat") {
mypalette=colorRampPalette(heat.colors(10)) # nota: my palette is a function.
}

if (palette.col=="jet") {
mypalette <- colorRampPalette(c("#00007F", "blue", "#007FFF", 
                                "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
}

image(1:dim(res)[1], 1:dim(res)[2], as.matrix(res), axes=T, add=TRUE, col=mypalette(10), zlim=myzlim)

axis(side=2, at=1:dim(res)[2], labels=colnames(res), las=1)
erp.xaxis(length.erp=dim(res)[1], startmsec=startmsec, endmsec=endmsec, x.tick=seq(startmsec, endmsec, 200)) 

}
