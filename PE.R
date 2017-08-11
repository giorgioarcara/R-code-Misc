###########################################
#### CALCOLO P.E. #########################
###########################################
library(tolerance)
x=1:100

PE=function(scores, inverse=FALSE){
	x=1:length(scores)
	require(tolerance)
	# calcolo il limite di tolleranza esterno che delimita almeno il 95% delle prestazioni migliori a destra.
	n.sogg.PE0=nptol.int(x, alpha=0.05, P=0.95)[3]
	# calcolo la percentuale di soggetti che corrispondono al limite del PE0. Mi serve per risalire al punto Z della distribuzione teorica dell'abilità misurata.
	perc.PE0=as.numeric(n.sogg.PE0)/length(x)
	#sapendo quanto è la percentuale di soggetti che mi aspetto che abbia un punteggio "non nella norma", riporto questa percentuale nella distribuzione teorica della variabile misurata, che SI ASSUME abbia una distribuzione normale.
	lim.sup.Z0=qnorm(perc.PE0)
# per garantire un intervallarità A LIVELLO DELLA VARIABILE TEORICA MISURATA, divido lo spazio tra il limite superiore e lo 0 (cioè la media e la mediana) in tre spazi uguali. Nota che se immagini una distribuzione normale dell'abilità (dove l'asse x sono i possibili valori dell'abilità), l'intervallarità si riferisce proprio alla larghezza degli intervalli di questo asse x. 
	interval.size.Z=abs(lim.sup.Z0/3)
#Aggiungo questo intervallo a tutti gli altri pezzi che mancano per ottenere i confini dei miei tre intervalli.
	lim.sup.Z1=lim.sup.Z0+interval.size.Z
	lim.sup.Z2=lim.sup.Z0+(2*interval.size.Z)
	lim.sup.Z3=lim.sup.Z0+(3*interval.size.Z)#questo deve essere 0 per definizione.
	
#a partire da questi confini calcolo la percentuali di soggetti che mi aspetto di trovare all'interno di ogni intervallo.
	perc.PE1=pnorm(lim.sup.Z1)-pnorm(lim.sup.Z0)
	perc.PE2=pnorm(lim.sup.Z2)-pnorm(lim.sup.Z1)
	perc.PE3=pnorm(lim.sup.Z3)-pnorm(lim.sup.Z2)
#riconduco la percentuale all'effettivo numero di soggetti rispetto al campione considerato
	n.sogg.PE1=round(perc.PE1*length(x))
	n.sogg.PE2=round(perc.PE2*length(x))
	n.sogg.PE3=round(perc.PE3*length(x))
	
#ordino i dati 
	sorted.data=sort(scores)
	if (inverse==TRUE){
		sorted.data=sort(scores, decreasing=TRUE)
		}
#faccio un passo indietro verso la non parametricità riferendomi alla serie ordinata dei dati. E trovando in altre parole quale valore (della serie ordinata dei dati), corrisponde al valore calcolato su scala intervallare della distribuzione teorica (normale) dell'abilità che intendo misurare.
# Ricapitolando immaginiamo di avere l'abilità "memoria" e che questa sia distribuita normalmente. Una volta stabilito il limite per la non-normalità (che è il limite di tolleranza esterno), divido lo spazio tra questo valore e il valore medio (che in una distribuzione normale è 0), in tre parti uguali. Calcolando la percentuale di soggetti (e di conseguenza il numero di soggetti) che c'è in ciascuno di questi intervalli. Posso creare un parallelismo tra serie ordinata dei punteggi osservati e punteggi dell'abilità teorica.

	PE0=sorted.data[as.numeric(n.sogg.PE0)]
	PE1=sorted.data[as.numeric(n.sogg.PE1)]
	PE2=sorted.data[as.numeric(n.sogg.PE2)]
	PE3=sorted.data[as.numeric(n.sogg.PE3)]

	# In accordo a quanto detto da Capitani (pag 75). L'osservazione corrispondente al limite superiore è considerata all'interno dell'intervallo che delimita. Ad esempio il punteggio corrispondente al soggetto del n.sogg.PE0 viene considerata come PE=0 (nelle formulazioni iniziali dei PE equivalenti il punteggio era escluso). Punteggi uguali o inferiori a quel valore saranno considerati deficitari.

	return(data.frame(PE0=PE0, PE1=PE1, PE2=PE2, PE3=PE3))
}
