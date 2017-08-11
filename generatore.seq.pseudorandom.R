############################################
# GENERAZIONE SEQUENZE PSEUDORANDOM ########
############################################

# per funzionare vanno specificate tre cose
# - variabile: variabile di partenza (un vettore o data.frame)
# - criterion: è un check logico per vedere se la sequenza rispetta il nostro criterio. Va in questa forma
				# criterion=sequenza[i]==sequenza[i-1]&sequenza[i]==sequenza[i-2]&sequenza[i]==sequenza[i-3]
# - min.num.step, cioè il minimo numero di step che devono essere completati prima  di poter fare il check 	(se per esempio il check è di confrontare quattro valori il min.num.step è 4)



variabile=(c(rep(12, 8), rep(14, 8), rep(16,8),rep(18,8))-2)*1000
min.num.step=4 # il minimo numero di step prima di fare dei check







nprove=0
variabile.pool=variabile
sequenza=NULL

	counter=0
	while (counter<min.num.step)
	{
	#### PRIMO STEP ###### selezione random quando il criterio non è attivo.
	index=sample(1:length(variabile.pool), 1)
	selezione=variabile.pool[index]
	sequenza=c(sequenza, selezione)
	variabile.pool=variabile.pool[-index]
	counter=counter+1
	}
	######
	#### SECONDO STEP ##### selezione random quando è attivo il criterio.

	while (length(sequenza)<length(variabile))
		{
		i=length(sequenza)
		index=sample(1:length(variabile.pool), 1) #nota che adesso pesca da variabile pool
		selezione=variabile.pool[index]
		nprove=nprove+1
		cat(".")
		##################
		### CRITERIO #####
		criterion=sequenza[i]==sequenza[i-1]&sequenza[i]==sequenza[i-2]&sequenza[i]==sequenza[i-3]
		#################
		##################
			if (!criterion)
				{
				sequenza=c(sequenza, selezione)
				}	
				if (nprove>=1000)	
				stop("mille tentativi fatti!!")
			}

	
	



