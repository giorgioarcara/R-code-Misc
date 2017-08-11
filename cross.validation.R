
########CROSS-VALIDATION#########
cross.validation=function(dat=dat, model=formula(dep~var), n=100, type="lm", method="test") {
crossval=matrix(ncol=n, nrow=round(((dim(dat)[1]/100*10))))
crossval=as.data.frame(crossval)
for (i in 1:n) {
selection=sample(dim(dat)[1], round(((dim(dat)[1]/100*90))))###sample of 90%
k=dat[selection,]
kout=dat[-selection,]
if (type=="gam") {
require(mgcv)
k.model=gam(model , data=dat)}
if (type=="lm") {
k.model=lm(model , data=dat)}
if (type=="reg.trees"){
require(rpart)
k.model=rpart(model, data=dat)	}
prediction=predict(k.model, kout)
dep=paste(model[[2]])
crossval[,i]=((prediction-kout[,dep])^2)}
if (method=="mean"){
MSE=mean(as.matrix(na.omit(crossval)))
}
if (method=="test"){
MSE=as.vector(as.matrix(na.omit(crossval)))
}
return(MSE)
}

compare.cross=function(dat=dat, model=formula(dep~var), n=100,n.sim=100, methods=c("lm", "reg.trees")){
	MSE.method1=NULL
	MSE.method2=NULL
	for (i in 1:n.sim){
		MSE.method1[i]=cross.validation(dat=dat, model=model,n=n, type=methods[1])
		MSE.method2[i]=cross.validation(dat=dat, model=model,n=n, type=methods[2])
			}
MSE.dat=data.frame(MSE.method1, MSE.method2)
names(MSE.dat)=methods
cat(wilcox.test(MSE.method1, MSE.method2))
invisible(MSE.dat)
}



