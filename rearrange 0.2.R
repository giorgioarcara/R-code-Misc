### REARRANGE  0.2###
# funzione per riarrangiare il datasetaset con variabili a misure ripetute
# da una struttura canonica (SPSS, STATISTICA), con una colonna per misura
# a una struttura tipica di R
# 
# deps= le colonne con le variabili dipendenti #MEGLIO METTERLE COME LISTA per risolvere il vecchio problema
# oth= le colonne con le altre variabili (non dipendenti)
# dataset= il datasetaset da cui prendere le colonne specificate prima
rearrange=function(deps, oth=NULL, dataset, name.dep="Dep", name.newvar="New_Var") {
	dep=as.vector(as.matrix(dataset[,deps]))
	others=NULL
	if (length(oth)>0){
		for (i in 1:dim(dataset[,deps])[2]){
			others=rbind(others, dataset[,oth])
		}
	}
	nuova.var=NULL
	for (i in 1:length(names(dataset[,deps]))) {
		nuova.var=c(nuova.var,rep(names(dataset[,deps])[i], dim(dataset[,deps])[1]))
		}
	dat=as.data.frame(cbind(dep,others))
	names(dat)[1]=paste(name.dep)
	dat[,paste(name.newvar)]=as.factor(nuova.var)
	return(dat)
}