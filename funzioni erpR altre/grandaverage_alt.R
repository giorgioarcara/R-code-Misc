# Questa versione alternativa di grandaverage doveva permettere (in teoria) di specificare quali soggetti ed elettrodi non includere nell'average.
# ho smesso di svilupparla perhcé si poteva risolvere semplicemnete con quella precedente di mettere NA (prestando attenzione e facendo backup)

grandaverage <-
#NOTA questa funzione può indurre in errore, perché ci sono NA

# in questa f(x) AGGIUNGI UN WARNING SE CI SONO NA e crea una funzione (da usare preliminarmente) che faccia un check di tutti i dati
# magari una funz veloce che ti dica solo se sono completi e una più dettagliata che ti dica invece range e eventuali NA.

function(base, numbers, electrodes="all", env=.GlobalEnv, Subj.ign=NULL, el.ign=NULL)
	#forse da togliere la possibilità di selezionare elettrodi.
	{
	comment_text=paste("Subjects averaged: ", paste(base,numbers[1], sep=""))
	average.temp=eval(parse(file="", text=paste(base,numbers[1], sep="")),env=env)
	
	if(electrodes[1]=="all"){
		electrodes=names(average.temp)
	}
	
	average.temp=average.temp[,electrodes]
	
	if(numbers[1]%in%Subj.ign)
			{
				average.temp[,el.ign]=0
			}

	
		for (i in 2:length(numbers))
		{
			average.temp.new=eval(parse(file="", text=paste(base,numbers[i],sep="")),env=env)[,electrodes]
			
			if(numbers[i]%in%Subj.ign)
			{
				average.temp.new[is.na(average.temp.new), el.ign]=0
			}
			
			average.temp=average.temp+average.temp.new
			
			comment_text=paste(comment_text,paste(base,numbers[i], sep=""),"\n")
				
			
		}
		Electrodes.n=rep(length(numbers), length(numbers)) # Electrodes.n è il numero di soggetti per cui gli elettrodi sono considerati
		average=average.temp/rep(Electrodes.n, each=nrow(average.temp))
		comment(average)=comment_text
		if (sum(Electrodes.n-(length(numbers)))!=0){ #nota: length(numbers) è il numero di soggetti. In questo modo recupero il numero di sogg con NA.
			warning("The average included some NA values. Use check.erp function", call.=FALSE)
		}
		return(average)
		}
