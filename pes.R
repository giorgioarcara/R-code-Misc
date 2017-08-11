# partial eta squared per ez 0.3
# - in input inserire la stessa sintassi per creare un modello ezANOVA 


pes=function(...){
	res=ezANOVA(..., detailed=TRUE)
	partial.eta=round(res$ANOVA$SSn/(res$ANOVA$SSn+res$ANOVA$SSd),2)
	Effect=res$ANOVA$Effect
	result=data.frame(Effect=Effect, partial.eta=partial.eta)
	return(result)
}