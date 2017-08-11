#########################################
# DIAGNOSTICS FUNCTION FOR OLS (DESIGN PACKAGE)
########################################
diagn=function(model, obs=NULL){
	rstand=as.vector(scale(resid(model)))
	fitt=fitted(model)
	par(mfrow=c(2,2))
	plot(density(na.omit(rstand)), main="standard residual distribution")
	qqnorm(rstand, cex=0.5, main="q-q plot")
	qqline(rstand)
	plot(na.omit(rstand)~fitt, pch=".",cex=2, main="fitted aganist residuals", ylim=range(c(-4,4),max(rstand, na.rm=T), min(rstand, na.rm=T)))
	abline(h=c(-2.5, 2.5))
	dffits=abs(resid(model, "dffits"))
	plot(dffits, type="h", main="difference in the fits")
	par(mfrow=c(1,1))
	quartz()
	if (!is.null(obs)){
		plot(na.omit(obs), fitt, xlab="observed", ylab="fitted", pch=19)}
	}