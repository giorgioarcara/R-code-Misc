### TIME.INFL
# questa funzione serve per identificare quali soggetti possono potenzialmente influenzare delle analisi rispetto ad una certa finestra temporale, avendo risultati particolarmente "estremi".
# dal momento che la funzione utilizza i quantili per il calcolo di valori outliers, essa troverà quasi sempre dei valori estremi ed è quindi da considerarsi poco conservativa. é utile accompagnare una conferma con le funzioni average.infl o butterfly.

#base = le prime lettere degli oggetti 
#numbers= il numero dei soggetti di cui calcolare l'average
# win.ini = in msec, l'inizio della finestra nella quale calcolare eventuali outliers
# winend = in msec, la fine della finestra nella quale calcolare eventuali outliers
# startmsec= in msec l'inizio della registrazione
# endmsec = in msec, la fine della registrazione.
# env= l'environment da cui recuperare i soggetti.


time.infl=function(base, numbers, win.ini, win.end, startmsec=-200, endmsec=1200, env=.GlobalEnv)
	{
	#definisco msectopoints
		
	msectopoints=function(a, lengthsegment, startmsec, endmsec){
	x=((a-(startmsec))*(lengthsegment-1))/(endmsec-(startmsec))
	return(x+1)}
		
		
	comment_text=paste("Subjects averaged: ", paste(base,numbers[1], sep=""))
	options(digits=10)
				
		all.subj=NULL #creo il dataset vuoto
		for (i in 1:length(numbers))
		{
			current.subj=eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env)
			subject.name=comment(current.subj) #recupero il nome del soggetto
			current.subj=colMeans(current.subj[round(msectopoints(win.ini,dim(current.subj)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(current.subj)[1],startmsec, endmsec)),])
			current.subj=current.subj^2

			current.subj=as.data.frame(t(current.subj))
			current.subj$Subject_name=subject.name
			current.subj$Subject=numbers[i]
			
			all.subj=rbind(all.subj, current.subj)
			
						
		}
		row.names(all.subj)=1:dim(all.subj)[1]
		
		
		#nota che il length(all.ave.excl) serve per escludere le colonne Subject_name e Subject, create nel ciclo precedente
		cutoff=apply(all.subj[1:(length(all.subj)-2)], 2, function(x){quantile(x, prob=0.95)})
		cutoff=as.data.frame(t(cutoff))
		
		for (k in 1:(dim(all.subj)[2]-2))
		{
			all.subj[,paste("check", names(all.subj)[k], sep="_")]=as.numeric(all.subj[,k]>=as.numeric(cutoff[k]))
			#nota l' as.numeric(cutoff[k]), è necessario mettere as.numeric altrimenti, essendo un elemento di un data.frame, mi fa un solo check
			# primo elemento contro primo elemento
			
		}
		
		return(all.subj)	
				
		}