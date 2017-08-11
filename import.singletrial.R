#funzione per importare file singletrial ottenuti con funzione anahita

import.singletrial=function(st.file, format){
	
	dat=read.table(st.file, header=TRUE, sep="\t")
	info=dat[,1:4]
	dati=dat[,5:(length(dat)-1)] #c'è una colonna finale di NA
	dati=as.matrix(dati)
	
	if (format=="list"){
		return(list(info=info, dati=dati))
		}
	if (format=="data.frame"){
	return(cbind(info,dati))
	}
	}