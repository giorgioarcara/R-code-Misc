#### funzioni da libro C. Barbaranelli

#indice asimmetria distribuzione di Pearson (pag. 22)

skew=function(x, test=FALSE){
	x.mean=mean(x)
	beta=((sum((x-x.mean)^3))/length(x))^2/((sum((x-x.mean)^2))/length(x))^3
	cat("\nPearson Asimmetry index\n")
	cat(beta)
	cat("\n")
	if (test==TRUE){
		err.st.skew=(6/length(x))^(1/2)
		statistic=beta/err.st.skew
		cat("skewness Z-value (anticonservative)\n")
		cat(statistic)
		}
	}
	
kurt=function(x, test=FALSE){
	x.mean=mean(x)
	beta2=(((sum((x-x.mean)^4))/length(x))/((sum((x-x.mean)^2))/length(x))^2)-3 
	# all'indice viene sottratto 3 ino modo che in caso di distribuzione normale sia pari a 0.
	cat("\nPearson Kurtosis index\n")
	cat(beta2)
	cat("\n")
	if (test==TRUE){
		err.st.kurt=(24/length(x))^(1/2)
		statistic=beta2/err.st.kurt
		cat("kurtosis Z-value (anticonservative)\n")
		cat(statistic)
	}
}