## questa funzione dato un vettore x. Controlla se x e gli n punti successivi superano il criterio. Quindi restituisce, l'indice del vettore che corrisponde al primo elemento di x che soddisfa la condizione.
# serve per calcolare latency.# NOTA!!! il criterio è soddisfatto se il numero è > (non >=)


nhigher <- function(x, n=5, criterion){
	vet=x
	dat=data.frame(index=1:length(vet))
	# creo un data.frame con gli indici. Mi serve per utilizzare la funzione apply.
	
	test=function(index,vet,n) {
	indices=c((index):(index+n-1))
	indices=indices[indices>0&indices<(length(vet))]
	response=((all(vet[indices]>criterion))&length(indices)==(n))
	return(response)
	}
	
	candidates.indices=apply(dat, 1, function(x){test(index=x, vet, n)})
	
	if(any(candidates.indices)){
	#candidates=vet[candidates.indices]
	#return(candidates[1]) 
	first=match(TRUE, candidates.indices)
	return(first)} else {
		return(NA)
	}
	}
