barplot.conf<-function(dep.var, group.vars, wid.var, data, yrange=default, type="se", space=c(0,0.5), beside=TRUE, lower.seg=FALSE, ...){
  
  dat=aggregate(data[,dep.var], as.list(data[, c(group.vars, wid.var)]), mean, na.rm=T)
  names(dat)[length(dat)]=dep.var
  
  dep=dat[,dep.var]
  group=dat[,group.vars]
  default=range(0,max(dep))
  tmean=tapply(dep, group, mean)
  se=function(x){sd(x)/sqrt(length(x))}
  #tsd=tapply(dep, group, sd)#da modificare con se!!!
  tse=tapply(dep, group, se) 

    tn=tapply(dep, group, length)
  if (type=="CI"){
    tu=tmean+qt(.975, df=tn-1)*tse #upper bound of confidence
    tl=tmean+qt(0.025, df=tn-1)*tse #lower bound
  }
  if (type=="se"){
    tu=tmean+tse
    tl=tmean-tse	
  }
  tbars=barplot(height=tmean, ylim=yrange, beside=beside, ...)
  
  if (lower.seg==TRUE){
  segments(x0=tbars, x1=tbars, y0=tl, y1=tu)
  segments(x0=tbars-.1, x1=tbars+0.1,y0=tl, y1=tl)
  segments(x0=tbars-.1, x1=tbars+0.1,y0=tu, y1=tu)
  }
  
  if(lower.seg==FALSE){
    segments(x0=tbars, x1=tbars, y0=tmean, y1=tu)
    segments(x0=tbars-.1, x1=tbars+0.1,y0=tu, y1=tu)
  }
  
  
}