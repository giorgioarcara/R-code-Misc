shade.eeg=function(e1,e2, start, end, startmsec=-200, endmsec=1200, col="gray"){
	
	lengthwhole=length(e1)
	
	msectopoints=function(a, lengthsegment, baseline, total.length){
	x=((a-(baseline))*(lengthsegment-1))/(total.length-(baseline))
	return(x+1)}
	
	start.pnts=msectopoints(start, lengthwhole, startmsec, endmsec)
	end.pnts=msectopoints(end, lengthwhole, startmsec, endmsec)
	
	polygon(c(start.pnts:end.pnts, end.pnts:start.pnts), c(e1[start.pnts:end.pnts], e2[end.pnts:start.pnts]), col=col)

	
	
}