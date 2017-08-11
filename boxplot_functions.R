###TWO FUNCTIONS TO ADD OBSERVATION NUMBER TO BOXPLOT######


bplot=function(vec, col="red")  {
	
	x=c(rep(1, length(boxplot.stats(vec)$out)))
	y=boxplot.stats(vec)$out
	label=which(vec==boxplot.stats(vec)$out)
	boxplot(vec, outline=F, ylim=c(-3,3)) # devi aggiustare il limite, altrimenti facendo un box plot e mettendo outline F, l'outlier viene tagliato fuori.
	text(x, y, labels=label, col=col)}
	

bplot=function(dep, dat, col="red"){
	x=c(rep(1, length(boxplot.stats(dat[,dep])$out)))
	for (i in 2:length(x)) {
		x[i]=x[i-1]+0.05}
	y=boxplot.stats(dat[,dep])$out
	label=paste(match(boxplot.stats(dat[,dep])$out, dat[,dep]))
	boxplot(dat[,dep], outline=F)
	text(x, y, labels=label, col=col)}