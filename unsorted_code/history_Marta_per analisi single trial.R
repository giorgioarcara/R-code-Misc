### COME PRIMA COSA SELEZIONA LA DIRECTORY IN CUI VUOI METTERE I FILE DI OUTPUT
# si può selezionare con cmd+d oppure utilizzando la riga sotto (togliendo il cancelletto che commento)


setwd("/Users/giorgioarcara/Desktop/Export NM")




##############################
# DEFINIZIONE FINESTRA AVERAGE
##############################
# in millisecondi

win.start=500
win.end=600










#####################################################################

filename=(file.choose())
dati=read.table(filename)
conditionname=substr(filename, nchar(filename)-9, nchar(filename)-4)


#dati=read.table("/Users/giorgioarcara/Desktop/Export NM/C_Dic1.dat")


##################################
#### IMPORTAZIONE DATI
##################################
names(dati)=c("Fp1", "Fp2", "F3", "F4", "C3", "C4", "P3", "P4", "O1", "O2", "F7", "F8", "T3", "T4", "T5", "T6", "Cz", "Fz", "Pz", "HEOG1", "HEOG2", "VEOG1", "VEOG2")


######################################################
# CREO LA VARIABILE CHE IDENTIFICA I TRIAL
######################################################
## un'epoca di 1200 msec totali corrisponde a 300 punti (250 di freq. campionamento).
trial=NULL
for (i in 1:(dim(dati)[1]/300)){
	trial=c(trial, rep(i,300))
	}

dati$trial=trial

dati.split=split(dati, dati$trial)
#######




startmsec=-200
endmsec=1000



msectopoints=function(a,lengthsegment){
x=((a-(startmsec))*lengthsegment)/(endmsec+abs(startmsec))
return(x)}

win.start.points=msectopoints(win.start, 300)
win.end.points=msectopoints(win.end, 300)
#######


results=matrix(ncol=23, nrow=length(table(dati$trial))) #23 sono il numero degli elettrodi
results=as.data.frame(results)
names(results)=names(dati)[1:23]

for (i in 1:length(dati.split))
	{
	dat=dati.split[[i]]
	dat=dat[-length(dat)] #tolgo la colonna con il numero del trial, non più necessaria.
	results[i,]=apply(dat[win.start.points:win.end.points,],2,mean)
	}


write.table(results, file=paste(conditionname,"results.txt", sep="_"),quote=F, row.names=F)








