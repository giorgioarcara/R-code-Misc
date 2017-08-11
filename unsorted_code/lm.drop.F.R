### function to drop non significant terms

lm.drop.F<-function(mod){
	
	mod.anova=anova((mod))[-dim(anova(mod))[1], ] #recupero i risultatai ANOVA (escluso l'ultimo, residuals)

	updated.mod=mod # at the first step updated mod is this
	
	if (dim(mod.anova)[1]>0){ # if there is only the Intercept the dimension would be 0.
		# 
		mod.terms=anova((mod))[-dim(anova(mod))[1], ] 

		while (any(mod.terms[,"Pr(>F)"]>0.05)){ #salto l'intercetta e check se c'è almeno un termsf > 0.05
		
			to.drop=rownames(mod.terms[mod.terms[,"Pr(>F)"]==max(mod.terms[,"Pr(>F)"]),]) # trovo il valore con p-value più alto.
			updated.mod=eval(parse(file="", text=paste("update(updated.mod, .~.-", to.drop, ")", sep="")))
			cat("VARIABLE: ", to.drop, "dropped.\n")
			mod.terms = anova((updated.mod))[-dim(anova(updated.mod))[1], ] 
			
			
		}
		invisible(updated.mod)
	
	} else {
	cat("only intercept in this model")
	invisible(mod)
	}
}
	