erp.latency <-
function(base, numbers, win.ini, win.end, erplist=NULL, startmsec=-200, endmsec=1200, others=NULL, format="long", name.dep="Dep", name.newvar="electrode", peak.fun=max, frac=1, ...)
	{
	
	# preliminary checks
	if (is.null(erplist)){
	stop("an erplist object containing ERP data frames must be specified!", call.=F)
	}
	
	#### object checks
	object.names=paste(base, numbers, sep="")
	if (any(!object.names%in%names(erplist))){
		missing.objects=object.names[!object.names%in%names(erplist)]
		missing.object.collist=paste(missing.objects, "\n", sep="")
		stop("The following objects are not contained in the erplist specified:\n", missing.object.collist, call.=F)
	}
	
	#### object checks
	if (frac<0|frac>1){
	  stop("frac should be a value within the range 0 - 1", call.=F)
	}
	

	
		datall=NULL		
			
		for (i in 1:length(numbers))
		{
		average.temp=erplist[[paste(base,numbers[i], sep="")]]
		rownames(average.temp)=1:dim(average.temp)[1]
		Subject_name=comment(erplist[[paste(base, numbers[i], sep="")]])
		
		# in the case a fractional is specified, before searching, the whole matrix with data
		# is scaled by the fractional value
		# (this is a trick such as "frac * the max", rarther "the max" is searched).
	
		
		if (frac < 1) {
		  average.temp.check = average.temp * frac
		} else {
		  average.temp.check = average.temp
		}
		
		# average.temp check is a matrix with the same dimensions of average.temp, but scaled
		
		average.temp.peak=apply(average.temp.check[round(msectopoints(win.ini,dim(average.temp.check)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(average.temp.check)[1],startmsec, endmsec)),],2, FUN=function(x){peak.fun(x, ...)})
    

		peak.pnts=NULL
		for (k in 1:length(average.temp)){
			
			if (!is.na(average.temp.peak[k])){ # the peak point is not searched if not found previously.
			  
			  # note that here, you must look for the closest point of average.temp in average.temp.check
			  # find actual peak
			  peak.pnts.k=which.min(abs( average.temp[round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec)) : round(msectopoints(win.end,dim(average.temp)[1],startmsec, endmsec)) , k] - average.temp.peak[k]) ) 
			  
				#peak.pnts.temp=as.numeric(rownames(average.temp[average.temp[,k]==average.temp.peak[k],]))
				#peak.pnts[k]=peak.pnts.temp[peak.pnts.temp>=round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec))&peak.pnts.temp<=round(msectopoints(win.end,dim(average.temp)[1],startmsec, endmsec))][1]
			    
				
				} else {
			peak.pnts[k]=NA
			}
			
			}
			
		peak.msec=pointstomsec(peak.pnts, dim(average.temp)[1], startmsec, endmsec)
		average.temp.lat=data.frame(t(peak.msec))
		names(average.temp.lat)=names(average.temp)#ripristino i nomi degli elettrodi, persi nei pasaggi
		average.temp.lat$Subject=numbers[i]
		average.temp.lat$Subject_name=Subject_name
		datall=rbind(datall, average.temp.lat)
		}
		rownames(datall)=1:dim(datall)[1]
		
	if (format=="wide"){
		return(datall)
	}
	
	if(format=="long"){
		
		temp=rearrange(deps=1:(length(datall)-2),oth=c("Subject", "Subject_name"),dataset=datall, name.dep=name.dep, name.newvar=name.newvar)
		# notice: length(datall)-2 because the last two columns are Subject and Subject_name
	}
	
	if (!is.null(others))
		{
			for (i in 1:length(others))
			{
			temp[, names(others)[i]]=others[i]
			}
		}
	return(temp)
	}
