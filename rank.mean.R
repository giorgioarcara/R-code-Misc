rank.mean<-function(dip, group)
{	
tabella<-data.frame(dip, group);
tabella<-na.exclude(tabella);
ranghitot<-rank(tabella$dip);
tabella<-data.frame(tabella,ranghitot);
tabella$group<-factor(tabella$group);
a<-tapply(tabella$ranghitot, tabella$group, mean)
#{results<-cbind(somma.ranghi1, somma.ranghi2); 
#results<-as.data.frame(results); colnames(results)<-c("R1", "R2"); 
#return(list("somma dei ranghi"=results))}}
plot(tabella$ranghitot~tabella$group, ylab= NULL, xlab="gruppi");
return(a);
}

