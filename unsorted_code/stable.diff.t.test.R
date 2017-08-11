stable.diff.t.test<-function(base1=NULL, base2=NULL, numbers1=NULL, numbers2=numbers1, startmsec=NULL, endmsec=NULL, paired=F, erplist1=NULL, erplist2=erplist1, crit.npoints=10, crit.msec=NULL, electrodes="all", to.exclude=NULL, interval=c(startmsec, endmsec), p.crit=0.05) {


# preliminary checks
	if (is.null(erplist1)|is.null(erplist2)){
	stop("two erplist objects (erplist1 and erplist2) containing ERP data frames must be specified!", call.=F)
	}
	
	# consistency checks for paired =T
	if (paired==TRUE&(length(numbers1)!=length(numbers2))){
	stop ("if paired == TRUE, numbers1 and numbers2 must have equal length.", call.=F)
	}
	
	#### object checks
	object.names1=paste(base1, numbers1, sep="")
	if (any(!object.names1%in%names(erplist1))){
		missing.objects1=object.names1[!object.names1%in%names(erplist1)]
		missing.object.collist1=paste(missing.objects1, "\n", sep="")
		stop("The following objects are not contained in the erplist1 specified:\n", missing.object.collist1, call.=F)
	}
		#### object checks
	object.names2=paste(base2, numbers2, sep="")
	if (any(!object.names2%in%names(erplist2))){
		missing.objects2=object.names2[!object.names2%in%names(erplist2)]
		missing.object.collist2=paste(missing.objects2, "\n", sep="")
		stop("The following objects are not contained in the erplist2 specified:\n", missing.object.collist2, call.=F)
	}
	




###
# STEP 1 LOAD DATA
#

# retrieve names from first subject.
if (electrodes[1]=="all"){
	electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])
	}

### to.exclude overwrite electrodes!
if (!is.null(to.exclude)){
	electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])[!names(erplist1[[paste(base1, numbers1[1], sep = "")]])%in%to.exclude]
	}

### select interval to be analyzed.
startpoint=round(msectopoints(interval[1], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
endpoint=round(msectopoints(interval[2], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))

or.dim1=dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1]
new.dim1=length(startpoint:endpoint)

# calculate exact interval analyzed
# the exact analyzed time window could differ from the one specified following approximations with msecotopoints.
# the exact values considered will be returned.
exact.interval.start=pointstomsec(startpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
exact.interval.end=pointstomsec(endpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)



# datall is a matrix with subject in columns and time x electrodes in rows.

datall1 = NULL
    for (i in 1:length(numbers1)) {
        x.temp = erplist1[[paste(base1, numbers1[i], sep = "")]][startpoint:endpoint, electrodes] #note that I select the electrodes
		
		  # add exception if only one electrode is included
        if(is.null(dim(x.temp)[1])){
        	x.temp=data.frame(x.temp)
        	names(x.temp)=electrodes
        }
		
		
		x.vec=unlist(x.temp)
		if (i == 1) {
	        datall1 = x.vec
	        }
        if (i != 1) {
    		datall1=cbind(datall1, x.vec)
        }
    }
    
datall2 = NULL
    for (i in 1:length(numbers2)) {
        x.temp = erplist2[[paste(base2, numbers2[i], sep = "")]][startpoint:endpoint, electrodes] #note that I select the electrodes
		
		 # add exception if only one electrode is included
        if(is.null(dim(x.temp)[1])){
        	x.temp=data.frame(x.temp)
        	names(x.temp)=electrodes
        }
		
		x.vec=unlist(x.temp)
		if (i == 1) {
	        datall2 = x.vec
	        }
        if (i != 1) {
    		datall2=cbind(datall2, x.vec)
        }
    }

## 
# STEP 2 T.TEST POINT by POINT
###    
  
allres=rep(NA, dim(datall1)[1]) # uso datall1 come riferimento ma la dimensione (i timepoints) saranno uguale a datall2
allres.t=rep(NA, dim(datall1)[1]) # creo matrice per valori di t

if (paired ==  FALSE) {
	df=length(datall1[1, ]) -1 + length(datall2[1, ]) -1  # remember that cols of datall are the subjects.
	t.crit=qt(0.025, df=df) # non lo uso per ora. Non sono manco sicuro sia giusto

	for (i in 1:dim(datall1)[1]){
	# res=t.test(datall1[i,], datall2[i, ], var.equal=T, paired=F)$p.value
	
	res.t=(mean(datall1[i, ])-mean(datall2[i, ]))/sqrt(var(datall1[i,])/length(datall1[i,])+var(datall2[i, ])/length(datall2[i,])) # independent samples.
	
	res=pt(abs(res.t), df=df, lower.tail=F)*2
	
	allres[i]=res #p-value
	
	allres.t[i]=res.t

	}
	
}

if (paired == TRUE) {
	df=length(datall1[1, ]) -1 
	t.crit=qt(0.025, df=df) # non lo uso per ora.

	for (i in 1:dim(datall1)[1]){

	res.t=mean((datall1[i, ]-datall2[i, ]))/sqrt((var(datall1[i, ]-datall2[i, ])/length(datall1[i, ])))
	# dependent samples
	
	res=pt(abs(res.t), df=df, lower.tail=F)*2

	
	allres[i]=res
	
	allres.t[i]=res.t

	}
}



# reconstruct the matrix

res.mat=matrix(allres, nrow=dim(x.temp)[1], ncol=dim(x.temp)[2], byrow=F)
res.t.mat=matrix(allres.t, nrow=dim(x.temp)[1], ncol=dim(x.temp)[2], byrow=F)


res.mat.log=res.mat< p.crit #  log sta per logical. ho fatto un check con i risultati (sig) di uno scalp.t e sono identici :)
res.mat.log=apply(res.mat.log, 2, as.numeric)


### Parte da incorporare 

# cambio per creare un vettore collassando.
res.mat.vec=apply(res.mat.log, 2, function(x){paste(x, collapse="")})

library(stringr)

sampling.rate=function(x, baseline=NULL, total.length=NULL){
	samp.rate=((dim(x)[1]-1)*1000)/(-baseline+total.length)
	return(samp.rate)
}

mysamp.rate=sampling.rate(erplist1[[paste(base1, numbers1[1], sep = "")]], baseline=startmsec, total.length=endmsec)

if (!is.null(crit.msec)){
	crit.npoints=round(crit.msec/(1000/mysamp.rate))
	}
	
# calculate the exact ms, to be returned
exact.crit.msec=crit.npoints*(1000/mysamp.rate)


# DEFINE CRITERION
# look for n consecutive points
mycriterion=paste(rep(1, crit.npoints), collapse="")


crit.mat=matrix(NA, nrow=dim(res.mat.log)[1], ncol=dim(res.mat.log)[2]) #nota che è la dimensione di ogni oggetto n timepoitns x m colonne

# crit list is the list with results in mseconds.
crit.list=list(NULL)
length(crit.list)=dim(res.mat.log)[2]
names(crit.list)=names(x.temp)

for (i in 1:length(res.mat.vec)){
	crit.res=str_locate_all(res.mat.vec[i], paste(mycriterion, "+", sep=""))[[1]] #!!!!! NOTA IL + serve per il regexp e trova anche altro. Nota anche l'[[1]] è per accedere all'oggetto che viene creato in una lista.
	
	# check se dimensioni risultati sono maggiori di 0
	if (dim(crit.res)[1]>0){
		for (k in 1:dim(crit.res)[1]){
			crit.mat[crit.res[k,1]:crit.res[k,2], i ]=TRUE
			crit.list[[i]][[k]]=pointstomsec(crit.res[k,], dim(res.mat.log)[1], startmsec=interval[1], endmsec=interval[2])
		}
	}
}
 
#fine parte da incorporare




## filtro i risultati  di res.t.mat a partire da crit.mat e riarrangio al volo.
filt.mat=matrix(res.t.mat[crit.mat], nrow=nrow(res.mat), ncol=ncol(res.mat))

#par(mar=c(5.1, 8.0, 4.1, 2.1))
#plot(0,0, xlim=c(1, dim(filt.mat)[1]), ylim=c(1, dim(filt.mat)[2]), type="n", xlab="", ylab="", axes=F, frame.plot=F)
#abline(v=1:dim(filt.mat)[1]-0.5, col="lightgray", lwd=0.3)
#abline(h=1:dim(filt.mat)[2]-0.5, col="lightgray", lwd=0.3)


# add names to filt.mat


## reconstruct a data.frame with the original number of time points before returning results
or.filt.mat=as.data.frame(matrix(rep(NA, or.dim1*dim(x.temp)[2]), nrow=or.dim1, ncol=dim(x.temp)[2]))
or.filt.mat[startpoint:endpoint, ]=filt.mat

or.t.mat=as.data.frame(matrix(rep(NA, or.dim1*dim(x.temp)[2]), nrow=or.dim1, ncol=dim(x.temp)[2]))
or.t.mat[startpoint:endpoint, ]=res.t.mat

colnames(or.t.mat)=names(x.temp)

colnames(or.filt.mat)=names(x.temp)



## sig (create a sig result to be feed to scalp.t)
sig.mat=!is.na(or.filt.mat)


param=data.frame(crit.npoints=crit.npoints, exact.crit.msec=exact.crit.msec, interval.start=interval[1], interval.end=interval[2], exact.interval.start=exact.interval.start, exact.interval.end=exact.interval.end, analyzed.npoints=dim(x.temp)[1])

allresults=list(param=param, t.mat=or.t.mat, results=crit.list, raster.res=or.filt.mat, sig=sig.mat)


# separa il plot in altra funzione?

par(mar=c(5.1, 8.0, 4.1, 2.1))
plot(0,0, xlim=c(1, dim(or.filt.mat)[1]), ylim=c(1, dim(or.filt.mat)[2]), type="n", xlab="", ylab="", axes=F, frame.plot=F)
abline(v=1:dim(or.filt.mat)[1]-0.5, col="lightgray", lwd=0.3) # note the shift -0.5 to center the little rectangles.
abline(h=1:dim(or.filt.mat)[2]-0.5, col="lightgray", lwd=0.3)



### GRAFICO

mypalette=colorRampPalette(heat.colors(10)) # nota: my palette is a function.

image(1:dim(or.filt.mat)[1], 1:dim(or.filt.mat)[2], -as.matrix(or.filt.mat), xaxs="r", yaxs="r", axes=F, add=TRUE, col=mypalette(10))

axis(side=2, at=1:dim(or.filt.mat)[2], labels=names(x.temp), las=1)
erp.xaxis(length.erp=dim(or.filt.mat)[1], startmsec=startmsec, endmsec=endmsec, x.tick=seq(-200, 1500, 200)) 




invisible(allresults)

}



