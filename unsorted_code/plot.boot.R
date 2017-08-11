# x è il vettore con i risultati del bootstrap

plot.boot=function(x, pch=23, bg="blue", col="blue", pos=6, cex=1, x.startpoint=0){
	points(grep(1,x)+x.startpoint, rep(pos,length(grep(1,x))), pch=pch, bg=bg, col=col, cex=cex)
}