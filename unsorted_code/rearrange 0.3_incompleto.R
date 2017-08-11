### REARRANGE  0.3###
# funzione per riarrangiare il datasetaset con variabili a misure ripetute
# da una struttura canonica (SPSS, STATISTICA), con una colonna per misura
# a una struttura tipica di R
# 
# deps= le colonne cone le variabili dipendenti #MEGLIO METTERLE COME LISTA
# oth= le colonne con le altre variabili (non dipendenti)
# dataset= il datasetaset da cui prendere le colonne specificate prima
rearrange=function(dataset,oth=NULL, deps, name.deps="default", name.newvar="default") {
if(!is.list(deps)){
	cat("Le dipendenti devono essere dichiarate in una lista!!")
	break 
	}
if(name.deps!="default"){
	if(length(deps)!=length(name.deps)){
	cat("Il numero di nomi dei fattori è diverso dal numero delle nuove dipendenti!")
	break}
if(name.newvar!="default"){
	if(length(deps)!=length(name.newvar)){
	cat("Il numero di nomi delle dipendenti è diverso dal numero delle nuove dipendenti!")
	break}
for (k in 1:length(deps)){	
	dep=as.vector(as.matrix(dataset[,deps[k]]))
	others=NULL
	if (length(oth)>0){
		for (i in 1:dim(dataset[,deps[k]])[2]){
			others=rbind(others, dataset[,oth])
		}
	}
