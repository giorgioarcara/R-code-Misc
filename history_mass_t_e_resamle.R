setwd("C:/Users/Mamma/Desktop/nuovi files/MEGHEM analisi marzo 2016/MEGHEM analisi R")


library(erpR)
data(ERPsets)

source("mass.t.test2.R")
source("plot.raster.R")


erplist=ERPsets

new_erplist=erplist

for (i in 1:length(erplist)){
  curr.obj=erplist[[i]]
  erpobj.names=names(curr.obj)
  new.el.length=length(decimate(curr.obj[,1], 2)) # resample usando decimate
  #initialize object
  new.erpobj=as.data.frame(matrix(NA, nrow=new.el.length, ncol=length(curr.obj)))
  for (k in 1:length(erplist[[i]])){
    new.erpobj[,k]=decimate(curr.obj[,k], 2)
  }
  names(new.erpobj)=erpobj.names
  new_erplist[[i]]=new.erpobj
  names(new_erplist)[i]=names(erplist)[i]
} 


for (i in 21:40){
  erplist[[i]]=erplist[[i]]+2
}


mass.res=mass.t.test2(base1="Exp1_word_subj", base2="Exp1_nonword_subj", numbers1=1:20, numbers2=1:20, paired=T, erplist1=erplist, erplist2=erplist, startmsec=-200, endmsec=1500, electrodes="all", p.adj="fdr", interval=c(0, 600) )


