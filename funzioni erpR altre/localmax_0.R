localmax <- function(x, n=5){
	vet=data.frame(x=x, index=1:length(x))
	xmax=max(vet$x)
	xmax.index=which(vet$x==xmax)
	if ( (xmax.index>n+1) & (xmax.index<dim(vet)[1]-n)){
		#se il punto è sufficientemente lontano dai margini non c'è bisogno di fare cicli inutili.
		return(xmax)
		} else {
	stop.criterion=0
	nplus=0
	nminus=0
	# per trovare il local max in accordo a come definito da Luck (2005, pag 231). è sufficiente che mi allontano dai margini finchè il criterio suggerito da luck (cioè che da 3 a 5 punti siano inferiori al max), sia soddisfatto
	while (stop.criterion==0) {
		if (xmax)
		xmax=max(vet[(incr+1):(dim(vet)[1]-(incr+1)), "x"])
		xmax.index=which(vet$x==xmax)
		criterion=!(any(vet[(xmax.index-n):(xmax.index-1), "x"]>xmax)|any(vet[(xmax.index+1):(xmax.index+n), "x"]>xmax))
		if (criterion) {
			stop.criterion=1
			return(xmax)
			} else {
			incr=incr+1
			}
		}
	}
}
