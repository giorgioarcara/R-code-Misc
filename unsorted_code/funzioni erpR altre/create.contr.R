### funzione per a partire da un vettore, una lista di contrasti (coppie) per la funzione t.test.pairs 

### forma a partire da un vettore c(a,b,c,d) list(list(a, b), list(c, d))...)
create.contr<-function(x){
	vet=matrix(x, ncol=2, byrow=TRUE)
	lis=apply(vet,1,as.list)
	return(lis)
}