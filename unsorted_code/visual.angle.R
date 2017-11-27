## con length.s si indica la lunghezza totale dello stimolo, centrato rispetto al soggetto.
#

visual.angle=function(angle=NULL, length.s=NULL, distance=NULL, plot=T){
  
  # remember. Recommended visual angle is 2 degrees (Fovea)
  
  
	if (is.null(angle)){
	## ang_gradi=ang_rad*(180/pi)
	# angle=angle.deg*pi*(180)
	angle=2*atan((length.s/2)/distance)
	invisible(angle)
	cat("angle = ", angle,"rad; ", angle*(180/pi),"°\n")
	}
	if (is.null(distance)){
	distance=tan(angle)*length.s
	invisible(distance)
	cat("distance = ", distance,"\n")
	}
	if (is.null(length.s)){
	length.s=tan(angle)*distance
	invisible(length.s)
	cat("lengt.s = ", length.s,"\n")
	}
}