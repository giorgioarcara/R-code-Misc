############################################
# GENERAZIONE SEQUENZA PSEUDORANDOM PER fMRI
############################################

intervals=(c(rep(12, 8), rep(14, 8), rep(16,8),rep(18,8))-2)*1000

#intervals=c(10, 10, 10, 10, 10, 10, 20)

sequenza.trovata=FALSE
nprove=0

while (sequenza.trovata==FALSE)
	{
	sequenza=sample(intervals, length(intervals), replace=F)
	for (i in 4:length(intervals))
		{
		if (sequenza[i]==sequenza[i-1]&sequenza[i]==sequenza[i-2]&sequenza[i]==sequenza[i-3])
			{
			sequenza.trovata=FALSE
			nprove=nprove+1
			cat(".")
			break
			}	
		}
	if (nprove==100)
		{
		stop("eseguite 100 prove")
		}
	if (i==length(intervals)&!(sequenza[i]==sequenza[i-1]&sequenza[i]==sequenza[i-2]&sequenza[i]==sequenza[i-3]))
		{
		sequenza.trovata=TRUE
		}	
	}
	


