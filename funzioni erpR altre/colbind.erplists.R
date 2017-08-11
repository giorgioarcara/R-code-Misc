colbind.erplists=function(base1=NULL, base2=NULL, numbers1=NULL, numbers2=numbers1, outname=base1, out.numbers=numbers1, erplist1=NULL, erplist2=NULL, erplist1.elec="all", erplist2.elec="all"){
  
  # initial_checks
  
  if (!is.numeric(numbers1)|!is.numeric(numbers2)) {
    stop("\"numbers\" must be a numeric vector", call. = F)
  }
  if (length(base1) > 1) {
    stop("the argument \"base1\" and \"base2\" must have length equal to 1",
         call. = F)
  }
  if (length(base2) > 1) {
    stop("the argument \"base1\" and \"base2\" must have length equal to 1",
         call. = F)
  }
  if (is.null(erplist1)|is.null(erplist2)) {
    stop("two erplist objects containing ERP data frames must be specified",
         call. = F)
  }
  if (is.null(outname)) {
    stop("the argument \"outname\" must be specified", call. = F)
  }
  
  if (length(numbers1)!=length(numbers2)){
    stop("The arguments \"numbers1\" and \"numbers2\" must have the same length", call.=F)
  }
  
  if (length(setdiff(numbers1, numbers2))>0){
    warning("The numbers (hence the subjects) you are collapsing are different\n,. Please check if this is not an error", call.=F)
  }
  if (erplist1.elec[1]!="all" & !erplist1.elec%in%names(erplist1[[1]])){
    stop("The electrode names you are specifying are not contained in the erplist1", call.=F)
  }
  
  if (erplist2.elec[1]!="all" & !erplist2.elec%in%names(erplist2[[1]])){
    stop("The electrode names you are specifying are not contained in the erplist2", call.=F)
  }
  
  
  erplist.res=list(NULL)
  length(erplist.res)=length(numbers1)
  
  if (erplist1.elec[1]=="all"){
    erplist1.elec=names(erplist1[[1]])
  }
  
  if (erplist2.elec[1]=="all"){
    erplist2.elec=names(erplist2[[1]])
  }
  
  
  for (i in 1:length(numbers1)){
    erplist.res[[i]]=cbind(as.data.frame(erplist1[[paste(base1, numbers1[i], sep = "")]][erplist1.elec]), as.data.frame(erplist2[[paste(base2, numbers2[i], sep = "")]][erplist2.elec]))
    names(erplist.res)[[i]]=paste(outname, out.numbers[i], sep="")
    
    
  }
  
  return(erplist.res)
  
}
