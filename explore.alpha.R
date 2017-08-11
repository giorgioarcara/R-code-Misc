# Function to look in the data for best alpha, dropping variables with worst alpha drop. one-by-one.

explore.alpha=function(x){
	go=TRUE
	count=0
	while (go==TRUE){
		x.alpha=alpha(x, check.keys=F) # calculate alpha
		x.alpha.ordered=x.alpha$alpha.drop[order(x.alpha$alpha.drop$raw_alpha, decreasing=T),] #order with worst alpha drop first
		worst.var=x.alpha.ordered[1,] # define worst variable
		worst.var.name=row.names(worst.var) # retrieve name of worst variable
		worst.var.alphadrop=worst.var$raw_alpha # retrieve alpha value of worst variable
		curr.alpha=x.alpha$total$raw_alpha # define current alpha
		count=count+1
		if (round(worst.var.alphadrop,2)<round(curr.alpha,2)){ # crucial check, if new alpha is <= of current alpha break, otherwise..
			go = FALSE
			return(x.alpha)
		} else {
		x=x[, -which(names(x)%in%worst.var.name)] # ... update the dataset dropping the variable and restar the cycle.
		cat("Item: ", worst.var.name, " REMOVED\n")
		}
	}
}