obs.lmer<-function(mod, dep.name){
	# mod is the name of the model
	# dep.name is a character object indicating the name of the dependent variable.
	return=mod@frame[, dep.name]
}