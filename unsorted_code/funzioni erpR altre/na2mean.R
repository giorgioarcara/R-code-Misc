#######################
# REPLACE NA TO MEAN
#######################


na2mean<-function(x, exclude=NULL){
	
	x=as.numeric(x)
	if (!is.null(exclude)){
		y=x[!x%in%exclude] } else {
		y=x
	}
	med<-mean(y, na.rm=T)
	y1<-replace(y,which(is.na(y)), med)
	
	if (!is.null(exclude)){
		x1=x
		x1[!x1%in%exclude]=y1
	} else {
	x1=y1	
	}
	
	return(x1)
	}




