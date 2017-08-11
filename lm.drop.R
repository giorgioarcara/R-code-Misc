### function to drop non significant terms

lm.drop<-function(mod){
	
	mod.coef=data.frame(summary(mod)$coefficients) #recupero i coefficienti del modello
	
	if (dim(mod.coef)[1]>1){
		
		## DEVI CAMBIARE!!! NON PUO' FUNZIONARE CON QUESTO CICLO
		# DEVE ESSERE UN WHILE E 
		updated.mod=mod
		
		mod.coef=data.frame(summary(updated.mod)$coefficients)
		mod.coef=mod.coef[(2:dim(mod.coef)[1]),]
		
		while (any(mod.coef[,4]>0.05)){ #salto l'intercetta e check se c'è almeno un coeff > 0.05
		
			to.drop=rownames(mod.coef[mod.coef[,4]==max(mod.coef[,4]),]) # trovo il valore con p-value più alto.
			updated.mod=eval(parse(file="", text=paste("update(updated.mod, .~.-", to.drop, ")", sep="")))
			cat("VARIABLE: ", to.drop, "dropped.\n")
			mod.coef=data.frame(summary(updated.mod)$coefficients)
			mod.coef=mod.coef[(2:dim(mod.coef)[1]),]
		}
		invisible(updated.mod)
	
	} else {
	cat("only intercept in this model")
	invisible(mod)
	}
}
	