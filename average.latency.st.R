### FUNZIONE PER FARE AVERAGE PEAK

#base1 = le prime lettere degli oggetti 
#numbers= il numero dei soggetti di cui calcolare l'average

#forse potresti mettera anche (come per la PLSgui) il sampling rate e la startmsec coem info da inserire.

average.latency.st=function(base, numbers, win.ini, win.end, env=.GlobalEnv, startmsec=-200, endmsec=1200, peaktype="max")
	{	
		datall=NULL		
			
		#### creo la funzione msectopoints
		## dati 1) il tempo iniziale 2) il tempo finale e 3) la lunghezza globale in punti converte l'argomento "a" da msec a punti
		msectopoints=function(a, lengthsegment, startmsec, endmsec){
		x=((a-(startmsec))*(lengthsegment-1))/(endmsec-(startmsec))
		return(x+1)}
		
		pointstomsec=function(a, lengthsegment, startmsec, endmsec){
			x=(((endmsec-(startmsec))*(a-1))/(lengthsegment-1))
			return(x)
			}
		####
		for (i in 1:length(numbers))
		{
		average.temp0=eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env)		
		info=average.temp0[,1:4]
		average.temp=t(average.temp0[,5:length(average.temp0)]) #devo girare perché mi servono i nomi di riga. (nei nomi di colonna non posso usare solo numeri)
		rownames(average.temp)=1:dim(average.temp)[1]
		average.temp.peak=apply(average.temp[round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(average.temp)[1],startmsec, endmsec)),],2, eval(parse(file="", text=peaktype)))
		
		peak.pnts=NULL
		data.win=average.temp[round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(average.temp)[1],startmsec, endmsec)),] #seleziono i dati nella finestra considerata
		for (k in 1:length(average.temp.peak)){
			if(!is.na(average.temp.peak[k])){
			peak.pnts.temp=which(abs(average.temp.peak[k]-data.win[,k])==min(abs(average.temp.peak[k]-data.win[,k])))[1] #questo script lo utilizzo per prendere il numero più vicino (per evitare che non trovi oil picco per differenze minime con cui il numero è codificato). Metto [1] perchè nell'improbabile caso (che mi è capitato...) ci siano due valori massimi uguali prenda il primo.
			peak.pnts[k]=as.numeric(rownames(data.win)[peak.pnts.temp])
			}
			if(is.na(average.temp.peak[k])){
			peak.pnts[k]=NA	
				}
			}
			
		peak.msec=pointstomsec(peak.pnts, dim(average.temp)[1], startmsec, endmsec)
		average.temp.lat=data.frame((peak.msec))
		names(average.temp.lat)="Ampl"
		window=rep(paste(win.ini, "_", win.end, sep=""), dim(average.temp.lat)[1])
		average.temp.lat=cbind(info, window, average.temp.lat)
		datall=rbind(datall, average.temp.lat)
		}
		rownames(datall)=1:dim(datall)[1]
		return(datall)
		}
		
		
		