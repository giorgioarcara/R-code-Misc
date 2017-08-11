#########################
# BACKFIT T < 2
##########################
#rapida funzione di backfit per oggetti .lmer
### il problema di questa funzione è trattare i fattori (per via della codifica dummy). Attualmente funziona solo con variabili continue
####
## inoltre gli indici si sballano tutti se ci sono variabili con più livelli perché ad ogni variabile non corrisponde una riga. In altre parole classes e coefs, non sono corrispondenti.



backfit.g=function(obj)
	{
	classes=data.frame(class=attr(attr(prova, "terms"), "dataClasses")[-1]) #nota tolgo la prima che è la variabile dipendente
	coefs=as.data.frame(summary(obj)@coefs) #estraggo i coefficienti dei fattori fissi
	coefs=coefs[2:dim(coefs)[1],]
	t.values=coefs[,"t value"] #estraggo i t values, 
	i=1
	while (sum(as.numeric(abs(t.values)<2))>1) #faccio andare il while finché almeno un |t| è minore di 2
		{		
		tobedropped.index=match(min(abs(t.values)), abs(t.values))
		tobedropped.name=rownames(coefs[tobedropped.index,])
		tobedropped.class=as.character(classes[tobedropped.index,"class"])		if (tobedropped.class=="numeric")
			{
			obj=eval(parse(file="", text=paste("update(obj, .~.-",tobedropped.name,")")))
			cat(i," dropped:", tobedropped.name, "\n")
			classes=data.frame(class=attr(attr(prova, "terms"), "dataClasses")[-1]) #nota tolgo la prima che è la variabile dipendente
			coefs=as.data.frame(summary(obj)@coefs) #estraggo i coefficienti dei fattori fissi
			coefs=coefs[2:dim(coefs)[1],]
			t.values=coefs[,"t value"] #estraggo i t values, nota che parto dal secondo per escludere l'intecetta
			i=i+1
			}
			
			if (tobedropped.class=="factor")
			{
			obj=eval(parse(file="", text=paste("update(obj, .~.-",tobedropped.name,")")))
			cat(i," dropped:", tobedropped.name, "\n")
			classes=data.frame(class=attr(attr(prova, "terms"), "dataClasses")[-1]) #nota tolgo la prima che è la variabile dipendente
			coefs=as.data.frame(summary(obj)@coefs) #estraggo i coefficienti dei fattori fissi
			coefs=coefs[2:dim(coefs)[1],]
			t.values=coefs[,"t value"] #estraggo i t values, nota che parto dal secondo per escludere l'intecetta
			i=i+1
			}
			
		}
	invisible(obj)
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	