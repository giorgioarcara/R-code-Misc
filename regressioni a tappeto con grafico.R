library(sfsmisc)


regressioni.a.tappeto.nico<- function (X,Y) {
 #funziona solo se le variabili hanno nomi diversi
 ifelse((is.matrix(X)|is.data.frame(X)),X<-X,X<-matrix(X))
 ifelse((is.matrix(Y)|is.data.frame(Y)),Y<-Y,Y<-matrix(Y))
 RIS<-NULL
 for (i in 1:ncol(X)) {
  for (j in 1:ncol(Y)) {
   k<-summary(lm(X[,i]~Y[,j]))
   h<-k[[4]][2,4]
   ris<-c(names(X)[i],names(Y)[j],format(h,scientific=F))
   RIS<-rbind(RIS,ris)
  }
 }
 RIS<-RIS[order(RIS[,3],decreasing=F),]
 RIS<-RIS[!is.na(RIS[,3]),]
 #return(RIS)
 a=RIS[RIS[,3]<.05,]
 altezza=trunc(sqrt(nrow(a)))
 larghezza=ceiling(nrow(a)/altezza)
 attach(X)
 attach(Y)
 par(mfrow=c(altezza,larghezza))
 for (i in 1:nrow(a)) {
	eval(parse(text=paste("plot(",a[i,1],",",a[i,2],",sub='",paste("p=",a[i,3]),"')")))
	eval(parse(text=paste("linesHyperb.lm(lm(",a[i,2],"~",a[i,1],"),col='blue')")))
	eval(parse(text=paste("abline(lm(",a[i,2],"~",a[i,1],"),col=2)")))
	}
}
