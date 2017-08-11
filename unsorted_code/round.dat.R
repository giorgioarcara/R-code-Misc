round.dat=function(dat,n)
	{
	dat=dat
	for (i in 1:length(dat))
		if(is.numeric(dat[,i]))
		{
		dat[,i]=round(dat[,i],n)	
		}
		return(dat)
	}