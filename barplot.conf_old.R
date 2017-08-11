###dovresti aggiungere la possibilità di mettere anche più variabili group e non solo una.
# 
# dep= la variabile dipendente, inserita come char
# group = la variabile gruppo, inserita come char.
# dat = il dataframe in cui sono contenute le variabili.



barplot.conf=function(dep, group, dat,yrange=default, main=NULL, col=c("blue"), type="se", ...) {
	dat=na.omit(dat[,c(dep, group)])
	dep=dat[,dep]
	group=dat[,group]
	default=range(0,max(dep))
	tmean=tapply(dep, group, mean)
	se=function(x){sd(x)/sqrt(length(x))}
	#tsd=tapply(dep, group, sd)#da modificare con se!!!
	tse=tapply(dep, group, se) 
	tbarHeight=matrix(tmean, ncol=length(levels(group)))
	tn=tapply(dep, group, length)
	if (type=="CI"){
		tu=tmean+qt(.975, df=tn-1)*tse #upper bound of confidence
		tl=tmean+qt(0.025, df=tn-1)*tse #lower bound
		}
	if (type=="se"){
		tu=tmean+tse
		tl=tmean-tse	
	}
	tbars=barplot(height=tbarHeight, beside=T, space=c(0,0.5), ylim=yrange, xpd=FALSE, names.arg=levels(group), axes=TRUE, main=main, col=col, ...)
	segments(x0=tbars, x1=tbars, y0=tl, y1=tu)
	segments(x0=tbars-.1, x1=tbars+0.1,y0=tl, y1=tl)
	segments(x0=tbars-.1, x1=tbars+0.1,y0=tu, y1=tu)
cat(tmean, "\n", tu, " ", tl)}
	
