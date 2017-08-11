msectopoints=function(a, lengthsegment, startmsec, endmsec){
	x=((a-(startmsec))*lengthsegment)/(endmsec+abs(startmsec))
	return(x)}