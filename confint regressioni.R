

x=rnorm(100)
y=x+rnorm(length(x),sd=1)
confidence.level=.99


mod.lm=lm(y~x)
#### CALCOLO VARIANZA RESIDUA MODELLI
residual.error=sqrt(deviance(mod.lm)/df.residual(mod.lm))
residual.error=sqrt(sum(resid(mod.lm)^2)/df.residual(mod.lm))



dat=data.frame(x=x,y=y)


xref=seq(-4,4,0.5)
conf=qt(confidence.level, mod.lm$df.residual)*residual.error*sqrt((1/length(xref))+(((xref-mean(x))^2)/sum((xref-mean(x))^2)))

ymean=predict(mod.lm, newdata=list(x=seq(-4,4,1)))

conf.upper=predict(mod.lm, newdata=list(x=xref))+conf
conf.lower=predict(mod.lm, newdata=list(x=xref))-conf

plot(x,y)
lines(xref,conf.upper)
lines(xref,conf.lower)
abline(mod.lm)





