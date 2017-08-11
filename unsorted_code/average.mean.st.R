### FUNZIONE PER FARE AVERAGE PEAK

#base1 = le prime lettere degli oggetti 
#numbers= il numero dei soggetti di cui calcolare l'average

#forse potresti mettera anche (come per la PLSgui) il sampling rate e la startmsec coem info da inserire.

average.mean.st=function(base, numbers, win.ini, win.end, env=.GlobalEnv, startmsec=-200, endmsec=1200)
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
		average.temp.mean=apply(average.temp[round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(average.temp)[1],startmsec, endmsec)),],2, mean)
		average.temp.mean=data.frame(average.temp.mean)
		names(average.temp.mean)="Ampl"
		window=rep(paste(win.ini, "_", win.end, sep=""), dim(average.temp.mean)[1])
		average.temp.mean=cbind(info, window, average.temp.mean)
		datall=rbind(datall, average.temp.mean)
		}
		rownames(datall)=1:dim(datall)[1]
		return(datall)
		}
		
		
		