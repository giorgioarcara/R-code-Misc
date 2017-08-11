export.cor.test<-function(var1, var2, data, filename="export.cor.txt", signif=2, method="pearson", type="line"){
	if (length(var2)>1){
		stop("specify only one variable for var2!", call.=F)
		}

	cor.temp=cor.test(data[, var1[1]], data[, var2], method=method)
	
	if (method=="spearman"){
	dat.temp=data.frame(var=var1[1], n=dim(na.omit(data[, c(var1[1], var2)]))[1], rho=cor.temp$estimate, p=cor.temp$p.value)
	} else {
	dat.temp=data.frame(var=var1[1], df=cor.temp$parameter, r=cor.temp$estimate, p=cor.temp$p.value)
	}
	
	if (length(var1)>1){
		for (i in 2:length(var1)){
			cor.temp=cor.test(data[, var1[i]], data[, var2], method=method)
			if (method=="spearman"){
				dat.temp.add=data.frame(var=var1[i], n=dim(na.omit(data[, c(var1[1], var2)]))[1], rho=cor.temp$estimate, p=cor.temp$p.value)
			} else {
				dat.temp.add=data.frame(var=var1[i], df=cor.temp$parameter, r=cor.temp$estimate, p=cor.temp$p.value)
			}
			dat.temp=rbind(dat.temp, dat.temp.add)
		}
	}
	rownames(dat.temp)=1:dim(dat.temp)[1]
	dat.temp[,2:length(dat.temp)]=signif(dat.temp[,2:length(dat.temp)], digits=signif)
	#return(dat.temp)
	if (type=="table"){
		write.table(dat.temp, file=filename)
	}
	if (type=="line"){
		fileConn<-file(filename, open = "w") #notice the open = "w", it allows to write multiple lines.

		for (i in 1:dim(dat.temp)[1]){
			
			if (method=="pearson"){
			curr.line=paste(dat.temp[i, "var"], "\t r(", dat.temp[i, "df"], ") = ", dat.temp[i, "r"], ", p = ", dat.temp[i, "p"])
			}
			if (method=="spearman"){
			curr.line=paste(dat.temp[i, "var"], "\t rho = ", dat.temp[i, "rho"], ", n = ", dat.temp[i, "n"], ", p = ", dat.temp[i, "p"])
			}
			writeLines(curr.line, con=fileConn)
		}
		close(fileConn)

	}
	
}