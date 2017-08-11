rank.mean.pair<-function(dip, group)
{
col1<-c(dip, group);
col2<-c(rep("1", length(dip)), rep("2", length(group)));
tabella<-data.frame(col1, col2);
tabella<-na.exclude(tabella);
ranghitot<-rank(tabella$col1);
tabella<-data.frame(tabella,ranghitot);
tabella$col2<-factor(tabella$col2);
a<-tapply(tabella$ranghitot, tabella$col2, mean);
plot(tabella$ranghitot~tabella$col2, ylab= NULL, xlab="gruppi");
return(a);
}

