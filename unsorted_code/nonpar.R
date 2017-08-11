### da aggiustare la parte within e il modo in cui vengono presentati i risultati (aggiungi invisible e un cat per i risultati.)
nonpar=function(formula, dat, corr=FALSE, paired=FALSE) {
if (paired==FALSE){
global=kruskal.test(formula, dat)
dep=dat[,paste(formula[[2]])]
fac=dat[,paste(formula[[3]])]
matrix.comb=combn(unique(as.character(fac)), 2)
post.hoc=list(NULL)
length(post.hoc)=dim(matrix.comb)[2]
for (i in 1:length(post.hoc)){
	post.hoc[[i]]=wilcox.test(dat[(dat[,paste(formula[[3]])]==matrix.comb[1,i]), paste(formula[[2]])], dat[(dat[,paste(formula[[3]])]==matrix.comb[2,i]), paste(formula[[2]])], paired=FALSE)}
global.results=data.frame(global[[1]][[1]], global[[2]][[1]], global[[3]][[1]])
names(global.results)=c("Kruskal_Chisq", "df", "p")
post.hoc.results=list(NULL)
length(post.hoc.results)=length(post.hoc)
for (i in 1:length(post.hoc)){
	res.temp=data.frame(post.hoc[[i]][[1]][[1]], post.hoc[[i]][[3]][[1]])
	names(res.temp)=c("W", "p")
	post.hoc.results[[i]]=res.temp
	names(post.hoc.results)[[i]]=paste(paste(matrix.comb[1,i]), "vs", paste(matrix.comb[2,i]))
	}
lista.results=list(global.results, post.hoc.results)
names(lista.results)[[1]]="ANALISI GLOBALE"
names(lista.results)[[2]]="POST-HOC"
return(lista.results)}
if (paired==TRUE){
global=friedman.test(formula)
dep=dat[,paste(formula[[2]])]
fac=dat[,paste(formula[[3]][[2]])]
within.=dat[,paste(formula[[3]][[3]])]
dat.agg=as.data.frame(tapply(dep, list(within., fac), mean))
matrix.comb=combn(1:length(dat.agg), 2)
post.hoc=list(NULL)
length(post.hoc)=dim(matrix.comb)[2]
for (i in 1:length(post.hoc)){
	post.hoc[[i]]=wilcox.test(dat.agg[,matrix.comb(1,i)], dat.agg[,marix.comb(2,i)], paired=TRUE)
	}

	}
return(list(global, post.hoc))
return(post.hoc[[i]])
}