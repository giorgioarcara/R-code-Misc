## NOTA!!! funziona solo con una var dipendente.

summarytab.export<-function(dep, groups,na.rm=TRUE, substr.l=max(nchar(levels(groups))), exp.name="new.table.txt"){
		
	groups=apply(as.data.frame(groups), 1, function(x){paste(x,collapse="_")})
	groups=as.factor(groups)
	levels(groups)=substr(levels(groups), 1,substr.l)

		sum.Ntot=as.numeric(table(groups))
		sum.Nvalid=as.numeric(table(groups[!is.na(dep)]))
		sum.NA=as.numeric(table(groups[is.na(dep)]))
		sum.mean=tapply(dep, groups, mean, na.rm=na.rm)
		sum.sd=tapply(dep, groups, sd,na.rm=na.rm)
		sum.max=tapply(dep, groups, max,na.rm=na.rm)
		sum.min=tapply(dep, groups, min,na.rm=na.rm)
		
	
	results=data.frame(MEAN=as.numeric(sum.mean), SD=as.numeric(sum.sd), MAX=as.numeric(sum.max), MIN=as.numeric(sum.min), Ntot=sum.Ntot, Nvalid=sum.Nvalid, NotAv=sum.NA)
	rownames(results)=levels(groups)
	write.table(results, file=exp.name, sep="\t", row.names=T, col.names=T, quote=FALSE)
		return(results)

	
}