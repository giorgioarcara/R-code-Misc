### FUNCTIONS TO REMOVE COLUMNS WITH VAR = 0.

rmvar0=function(x){
	if (dim(x)[2]<=1){
		stop("the object should have at least two columns")
	}
	x.var=apply(x, 2, function(x){var(x)==0})
	var0.cols=which(x.var)
	if (length(var0.cols)>0){
	x.res=x[, -var0.cols]
	cat("The following columns were removed:", names(var0.cols), sep="\n")
	} else {
	cat("no zero variance columns")
	x.res=x
	}
	return(x.res)
}
