## lo scopo di questa funzione è creare un nuovo dataset in cui non h opiù i timepoints, ma finestre mediate.
# questo mi può aiutare nell'esplorazione di effetti statistici (visto che presumibilmente opererò poi su finestre e non su timepoints)
# 

# l'output è quindi una erplist con dei "pseudo-ERP" fatti di finestre collassate.

# nota che in collapse.timepoints i punti perdono un po' di signficato, perchÃ¨ in realtÃ  ogni punto (es. 100) indica in realtÃ  la media da 100 a 200.
# pensa a come trattare questo aspetto.

## 

collapse.timepoints <-
function(bases, numbers,  erplist=NULL, startmsec=NULL, endmsec=NULL, window=100, interval=c(startmsec, endmsec), electrodes="all", to.exclude=NULL)
	{
	
	# preliminary checks
	if (!is.numeric(numbers)){
	stop("\"numbers\" must be a numeric vector", call.=F)
	}
	if (is.null(erplist)){
	stop("an erplist object containing ERP data frames must be specified", call.=F)
	}

	
		#### object checks
	object.names=c(paste(bases, numbers, sep=""))
	if (any(!object.names%in%names(erplist))){
		missing.objects=object.names[!object.names%in%names(erplist)]
		missing.object.collist=paste(missing.objects, "\n", sep="")
		stop("The following objects are not contained in the erplist specified:\n", missing.object.collist, call.=F)
	}
	
	# define startpoint and endpoint
	### select interval to be analyzed.
	startpoint=round(msectopoints(interval[1], dim(erplist[[paste(bases[1], numbers[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
	endpoint=round(msectopoints(interval[2], dim(erplist[[paste(bases[1], numbers[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))

	# retrieve electrodes labels
	# retrieve names from first subject.
	if (electrodes[1]=="all"){
	electrodes=names(erplist[[paste(bases[1], numbers[1], sep = "")]])
	}

	### to.exclude overwrite electrodes argument!
	if (!is.null(to.exclude)){
	electrodes=names(erplist[[paste(bases[1], numbers[1], sep = "")]])[!names(erplist[[paste(bases[1], numbers[1], sep = "")]])%in%to.exclude]
	}
	
	# convert window in points
	sampling.rate=function(x, baseline=NULL, total.length=NULL){
	samp.rate=((dim(x)[1]-1)*1000)/(-baseline+total.length)
	return(samp.rate)
	}
	
	mysamp.rate=sampling.rate(erplist[[paste(bases[1], numbers[1], sep = "")]], baseline=startmsec, total.length=endmsec)
	
	window.npoints=round(window/(1000/mysamp.rate))

	exact.window.msec=window.npoints*(1000/mysamp.rate)
	
	## create time windows in points
	
	window.ini=seq(startpoint, endpoint-window.npoints, window.npoints)	
	window.end=seq(startpoint+window.npoints, endpoint, window.npoints) # non so se questo -1 Ã¨ giusto.
	
	window.ini.msec=pointstomsec(window.ini, dim(erplist[[paste(bases[1], numbers[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
	window.end.msec=pointstomsec(window.end, dim(erplist[[paste(bases[1], numbers[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
		
	
	outlist=list()
	length(outlist)=length(numbers)*length(bases)
	
	n=1
	
	for (k in 1:length(bases))
	{
	for (i in 1:length(numbers))
		{
		res.dat=matrix(NA, nrow=length(window.ini), ncol=length(electrodes))
		curr.dat=erplist[[paste(bases[k], numbers[i], sep = "")]][, electrodes]
			for (j in 1:length(window.ini)){
				res.dat[j, ]=apply(curr.dat[window.ini[j]:window.end[j],], 2, mean)
			}
		res.dat=as.data.frame(res.dat)
		outlist[[n]]=res.dat
		names(outlist[[n]])=electrodes
		names(outlist)[[n]]=paste(bases[k], numbers[i], sep = "")
		n=n+1
		}
	}
	print(window.ini.msec)
	print(window.end.msec)

return(outlist)

}

