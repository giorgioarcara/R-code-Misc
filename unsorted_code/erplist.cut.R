erplist.cut<-function(bases, numbers, win.ini, win.end, erplist=NULL, startmsec=-200, endmsec=1200, others=NULL, startpoint=NULL, endpoint=NULL)
{
  
  # preliminary checks
  if (is.null(erplist)){
    stop("an erplist object containing ERP data frames must be specified!", call.=F)
  }
  
  #### object checks
  object.names=c(paste(bases, numbers, sep=""))
  if (any(!object.names%in%names(erplist))){
    missing.objects=object.names[!object.names%in%names(erplist)]
    missing.object.collist=paste(missing.objects, "\n", sep="")
    stop("The following objects are not contained in the erplist specified:\n", missing.object.collist, call.=F)
  }
  
  
  outlist=list()
  #length(outlist)=length(numbers*length(bases))
  
  for (k in 1:length(bases)){
    base=bases[k]
    for (i in 1:length(numbers))
    {
      x.temp=erplist[[paste(base,numbers[i], sep="")]]
      Subject_name=comment(erplist[[paste(base,numbers[i], sep="")]])
      if (is.null(startpoint)|is.null(endpoint)){
      x.temp=x.temp[round(msectopoints(win.ini,dim(x.temp)[1],startmsec, endmsec)):
                      round(msectopoints(win.end,dim(x.temp)[1],startmsec, endmsec)),]
      } else {
        x.temp=x.temp[ startpoint:endpoint,] # in case the exact points are specified.
      }
      
      x.temp=list(x.temp)
      names(x.temp)=paste(base,numbers[i], sep="")
      #
      outlist=c(outlist, x.temp)
      }
  }
  return(outlist)
}