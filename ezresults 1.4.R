ezresults=function(mod)
	{
	if(length(mod)==1)
	{
	stop("che cazzo utilizzi la funzione? non ci sono test di sfericità!", call.=F)
	}
	anovares=mod[[1]]
	anovasph=mod[[2]]
	anovacorr=mod[[3]]
	
	anovares=anovares[anovares[,"p<.05"]=="*",]
	anovares[,c(4:6)]=round(anovares[,c(4:6)],2)
	anovares[,7]=signif(anovares[,7],2)
	
	
	anovasph=anovasph[rownames(anovasph)%in%rownames(anovares),]
	
	res=anovares[,c(1:3,6:9)]
	
	res$sphericity.sig=""
	res[rownames(anovares)%in%rownames(anovasph),"sphericity.sig"]=anovasph[,"p<.05"]
	
	
	anovacorr=anovacorr[rownames(anovacorr)%in%rownames(anovasph),]
	
	res$p.corrGG=""
	res$sig.GG=""
	res$p.corrHF=""
	res$sig.HF=""
	
	res[rownames(anovares)%in%rownames(anovacorr), "p.corrGG"]=signif(anovacorr[,"p[GG]"],2)
	res[rownames(anovares)%in%rownames(anovacorr), "sig.GG"]=anovacorr[,"p[GG]<.05"]

	res[rownames(anovares)%in%rownames(anovacorr), "p.corrHF"]=signif(anovacorr[,"p[HF]"],2)
	res[rownames(anovares)%in%rownames(anovacorr), "sig.HF"]=anovacorr[,"p[HF]<.05"]

	return(res)	
	}