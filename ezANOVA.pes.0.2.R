# ezANOVA con partial eta.squared
#  
# input esattamente come ezANOVA.


ezANOVA.pes=function(...){
	# in this first step I make access to ... that is a list
	arguments=deparse(substitute(list(...)))
	# i transform in a single cell vector
	arguments=paste(arguments, collapse="")
	# I remove some part of the char string I don't need and that are created by the previous steps.
	arguments=gsub("^list\\(","",arguments)
	arguments=gsub("\\)$","",arguments)
	code=paste("res=ezANOVA(",arguments,", detailed=TRUE)")
		
	eval(parse(file="",text=code))
	partial.eta=round(res$ANOVA$SSn/(res$ANOVA$SSn+res$ANOVA$SSd),2)
	names(res$ANOVA)[length(names(res$ANOVA))]="pes"
	res$ANOVA$pes=partial.eta
	return(res)
	
}