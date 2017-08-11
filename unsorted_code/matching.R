#CORREGGI PER TOGLIERE NA e mettere medie anche in oggetto di output!!!

#############################################
###FUNZIONE PER VERIFICARE MATCHING CON ANOVA 
#############################################
# variabili = specifica il numero delle colonne delle quali vuoi fare le analisi (non semplicemente il nome!!)
# fattore = specifica il numero colonna con il fattore
# dataset = specifica il dataframe da cui prendere colonne e fattore.

matching=function(variabili, fattore, dataset) {
	dat=dataset
	risultati=list(NULL)
	length(risultati)=length(dat[,(variabili)])
	for (i in 1:length(dat[,(variabili)])) {
		formula.aov=paste("aov(",names(dat[,variabili])[i], "~", names(dat)[fattore],",data=dat",")", sep="")
		anov=eval(parse(file="", text=formula.aov))
	 	F_test=round(summary(anov)[[1]][[4]][1],3)
		p=signif(summary(anov)[[1]][[5]][1],3)
		dfnum=summary(anov)[[1]][[1]][1]
		dfden=summary(anov)[[1]][[1]][2]
	 	dat.table.source=dat
	 	dat.table.source[,fattore]=as.factor(as.character(dat.table.source[,fattore]))
	 	dat.table=tapply(dat.table.source[,variabili][,i], dat.table.source[,fattore], mean, na.rm=TRUE)
	 	dat.table.sd=tapply(dat.table.source[,variabili][,i], dat.table.source[,fattore], sd, na.rm=TRUE)

	 	risultati[[i]][["Risultati"]]=data.frame(F_test,p, dfnum, dfden)
	 	names(risultati)[i]=names(dat[,variabili])[i]
	 	risultati[[i]][["Medie"]]=dat.table
	cat("\n","\n")
	cat("********************** Risultati",names(dat[,variabili])[i],"**************************","\n")
	cat("F =    ",F_test,"\n")
	cat("df num = ",dfnum,"\n")
	cat("df den = ",dfden,"\n")
	cat("p = ",p,"\n", "\n")
	cat("MEDIE","\n")
	cat(print(dat.table)[0])
	cat("SD", "\n")
	cat(print(dat.table.sd)[0])
	cat("***********************************************************")
	}
invisible(risultati)
}
