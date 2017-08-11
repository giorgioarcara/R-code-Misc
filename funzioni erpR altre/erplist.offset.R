erplist.offset<-function(base, numbers, win.ini, win.end, erplist=NULL, startmsec=-200, endmsec=1200, others=NULL)
{
  # this funciton remove a offset (i.e. baseline correction, specified in win.ini, win.end)
  
  
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
  
  # calculate points of baseline correction
  points.ini = round(msectopoints(win.ini,dim(erplist[[paste(base,numbers[1], sep="")]])[1],startmsec, endmsec))
  points.end = round(msectopoints(win.end,dim(erplist[[paste(base,numbers[1], sep="")]])[1],startmsec, endmsec))
  
  outlist=NULL		
  for (i in 1:length(numbers))
  {
    x.temp=erplist[[paste(base,numbers[i], sep="")]]
    Subject_name=comment(erplist[[paste(base,numbers[i], sep="")]])
    x.temp=apply(x.temp, 2, function(x){x - mean( x[points.ini : points.end])})

    comment(x.temp)=paste(comment(erplist[[paste(base,numbers[i], sep="")]]), 
                            " ; removed offset: ", win.ini, " - ", win.end, sep="")
    outlist[[i]]=x.temp
    names(outlist)[[i]]=paste(base,numbers[i], sep="")    
  }
  
    return(outlist)
}
