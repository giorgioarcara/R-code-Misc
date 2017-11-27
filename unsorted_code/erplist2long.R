erplist2long<-function(base, numbers, erplist=NULL, startmsec=-200, endmsec=1200, others=NULL, name.dep="Dep", name.newvar="electrode")
{
  
  # This functions start from a traditional erplist structure and turn it into a long structure.
  # I.e., a data.frame with a time point for each row, each electrode, and each subject.
  
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
  
  
  datall=NULL		
  for (i in 1:length(numbers))
  {
    x.temp0=erplist[[paste(base,numbers[i], sep="")]]
    Subject_name=comment(erplist[[paste(base,numbers[i], sep="")]])
    ntimepoints=dim(x.temp0)[1] # get number of timepoints
    timevec = seq(startmsec, endmsec, length=ntimepoints) # create vector of timepoints
    nelectrodes=dim(x.temp0)[2] # get number of electerodes
    elnames = colnames(x.temp0) # get electrode names
    x.temp1=unlist(x.temp0)
    x.temp2=data.frame(Dep = x.temp1, name.newvar=rep(elnames, each=ntimepoints), timepoints=rep(timevec, ))
    x.temp2$Subject=numbers[i]
    x.temp2$Subject_name=Subject_name
    datall=rbind(datall, x.temp2)
  }
  rownames(datall)=1:dim(datall)[1]
  
  
  if (!is.null(others))
  {
    for (i in 1:length(others))
    {
      datall[, names(others)[i]]=others[i]
    }
  }
  names(datall)[c(1,2)]=c(name.dep, name.newvar)
  datall$Subject=as.factor(datall$Subject)
  return(datall)
}
