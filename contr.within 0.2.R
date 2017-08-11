###CONTRASTI WITHIN###
###################################
#la funzione richiede il dataset
#con la struttura tipica di R
# (una riga per osservazione)
#
# dep = numero di colonna con la variabile dipendente
# factors = numero di colonna/e con i fattori within
# subject = la colonna con 
# dat = il dataset da cui prendere i dati
# il vettore con i contrasti


contrw=function(dep, subj , fac , contr, dataset){
###in questa prima parte si ricodificano (le variabili) eventualemente collassando le diverse within
	dat=as.matrix(dataset[,fac])
	if (dim(dat)[2]>1) {
		fattori=dat[,1]
		for (i in 2:dim(dat)[2]){
			fattori=paste(fattori, dat[,i], sep="_")
			}
	}
	if (dim(dat)[2]==1) {
		fattori=dat
		}
	matrice=as.matrix(tapply(dataset[, dep], list(dataset[, subj], fattori), mean, na.rm=TRUE))
### questa seconda parte, presa dalla funzione di Gianmarco (e modificata nella parte dell'output), calcola i contrasti
	y<-matrice %*% contr
	val.contr<-as.numeric(t.test(y)$estimate)
	tval<-as.numeric(t.test(y)$statistic)
	df.tval<-as.numeric(t.test(y)$parameter)
	p.tval<-as.numeric(t.test(y)$p.value)
	##output
	cat("\n","\n")
	cat("********************** Risultati **************************","\n","\n")
	cat("Contrasto inserito:    ",contr,"\n")
	cat("Nomi delle variabili:   ",paste(contr[contr!=0],colnames(matrice)[contr!=0], sep=" "),"\n")
	cat("Media del contrasto =",val.contr,"\n")
	cat("t =",tval,"     df = ",df.tval,"     p =",p.tval,"(due code)","\n","\n")
	cat("***********************************************************","\n","\n")
}

#######
##########FUNZIONE PER RICAVARE I COEFFICIENTI#####
#######
#Questa funzione serve per capire l'ordine dei livelli per poter assegnare i coefficienti nella funzione dei contrasti

codici=function(fac, dataset){ 
	dat=as.matrix(dataset[,fac])
	if (dim(dat)[2]>1) {
		fattori=dat[,1]
		for (i in 2:dim(dat)[2]){
			fattori=paste(fattori, dat[,i], sep="_")
			}
	}
	if (dim(dat)[2]==1) {
		fattori=dat
		}
return(levels(as.factor(fattori)))
}
	
coeff.contrast=function(x, group1, group2) {
x=levels(as.factor(x))
coefficients=c(rep(0, length(x)))
indici=c(1:length(x))
	for (i in 1:length(group1)){
	coefficients[indici[x==group1[i]]]=1/length(group1)
	}
	for (i in 1:length(group2)){
	coefficients[indici[x==group2[i]]]=-1/length(group2)
	}
print(coefficients) 
}

