### FUNZIONE PER RIARRANGIARE UN DATASET E CREARE 
# dat= il dataset
# dep= la colonna con la variabile dipendente in formato lungo
# with= le colonne con le variabili within (in formato lungo, come da esito di rearrange)
# id= l'identificativo per soggetto.

man.rearrange=function(dat, dep, wit, id){
	dat.newcols=dat[,c(wit, dep)]
	newvar=apply(dat.newcols[1:length(wit)], 1,function(x){paste(x, collapse="_")})
	
	iData=dat.newcols[match(unique(newvar), newvar),wit]
	
	new.dat=data.frame(tapply(dat[,dep], list(dat[,id],newvar), mean))
	new.dat$id=rownames(new.dat)
	return(list(new.dat, iData))
	
	}