localmin <- function(x, n=5){
	vet=data.frame(x=x, index=1:length(x))
	xmin=min(vet$x)
	xmin.index=which(vet$x==xmin)
	if ( (xmin.index>n+1) & (xmin.index<dim(vet)[1]-n)){
		#se il punto è sufficientemente lontano dai margini non c'è bisogno di fare cicli inutili.
		return(xmin)
		} else {
	stop.criterion=0
	# per trovare il local min in accordo a come definito da Luck (2005, pag 231). è sufficiente che mi allontano dai margini finchè il criterio suggerito da luck (cioè che da 3 a 5 punti siano inferiori al min), sia soddisfatto
	incr=n
	while (stop.criterion==0) {
		xmin=min(vet[(incr+1):(dim(vet)[1]-incr), "x"])
		xmin.index=which(vet$x==xmin)
		criterion=!(any(vet[(xmin.index-n):(xmin.index-1), "x"]<xmin)|any(vet[(xmin.index+1):(xmin.index+n+1), "x"]<xmin))
		if (criterion) {
			stop.criterion=1
			return(xmin)
			} else {
			incr=incr+1
			}
		}
	}
}
