xyplot(RT~Trial|Subject, data=lexdec3, panel=function(x,y,subscripts){
	panel.xyplot(x,y)
	subject=as.character(lexdec3[subscripts[1], "Subject"])
	coefs=as.numeric(unlist(coef(lexdec3.lmer)$Subject[subject,]))
	panel.abline(coefs, col="black", lty=2)
	coefs=as.numeric(unlist(coef(lexdec3.lmerA)$Subject[subject,]))
	panel.abline(coefs, col="black", lty=1)
	})