compare.names<-function(dat1, dat2, type="not"){
# questa funzione serve per confrontare i nomi di due dataset per vedere eventuali mismatch.
# se type="not" allora restituisce i nomi non condivisi. Qualsiasi altra specificazione di type restituisce i nomi condivisi.
	if (type=="not"){
		
		cat("names in dat1 NOT in dat2:\n")
		return(names(dat1)[!names(dat1)%in%names(dat2)])
		} else {
			
		cat("names dat1 shared in dat2:\n")
		return(names(dat1)[names(dat1)%in%names(dat2)])
		}
}