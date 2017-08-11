# ezANOVA con partial eta.squared
#  
# input esattamente come ezANOVA.


ezANOVA.pes=function(...){
	res=ezANOVA(..., detailed=TRUE)
	partial.eta=round(res$ANOVA$SSn/(res$ANOVA$SSn+res$ANOVA$SSd),2)
	names(res$ANOVA)[length(names(res$ANOVA))]="pes"
	res$ANOVA$pes=partial.eta
	return(res)
}