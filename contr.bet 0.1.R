################################################################################################################################
#### contr.bet: funzione per il calcolo di un contrasto between
#### in input vanno forniti:
#### dep = nome (testo), della variabile dipendente
#### bet = vettore con nomi (testo), della variabile delle variabile between considerate.
#### bet1 = vettori con i nome (testo) delle variabile da contrastare. Se ci sono più variabili, i contrasti sono da riportare in questo modo ("livello1a_livello2b"), con i vari livelli separati da un underscore
#### bet2 = vettori con i nome (testo) delle variabile da contrastare. Se ci sono più variabili, i contrasti sono da riportare in questo modo ("livello1a_livello2b"), con i vari livelli separati da un underscore
#### dati = il dataset da cui recuperare le variabile.
### NOTA: ho fatto delle prove e i risultati di questi contrasti sono gli stessi che si ottengono con il programma STATISTICA. Ho fatto prove anche con più di un fattore e funziona.

contr.bet<-function(dep,bet, bet1, bet2, dati){
dataset=dati
dat=as.matrix(dataset[,bet])
	if (dim(dat)[2]>1) {
		fattori=dat[,1]
		for (i in 2:dim(dat)[2]){
			fattori=paste(fattori, dat[,i], sep="_")
			}
	}
	if (dim(dat)[2]==1) {
		fattori=dat
		}
dataset$nuovo_fattore=as.factor(fattori)
livelli_nuovo_fattore=levels(as.factor(fattori))
tabella=tapply(dataset[,dep], dataset[,"nuovo_fattore"], mean)
coefficients=c(rep(0, length(livelli_nuovo_fattore)))
indici=c(1:length(livelli_nuovo_fattore))
for (i in 1:length(bet2)){
	coefficients[indici[livelli_nuovo_fattore==bet2[i]]]=-1/length(bet2)
	}
	for (i in 1:length(bet1)){
	coefficients[indici[livelli_nuovo_fattore==bet1[i]]]=1/length(bet1)
	}
cb=coefficients

##########################################
######TABELLA CON MEDIE DI RIEPILOGO####
#########################################

MediaGruppo1=mean(dataset[fattori==bet1, dep], na.rm=TRUE)
MediaGruppo2=mean(dataset[fattori==bet2, dep], na.rm=TRUE)


### PARTE ADATTATA DA FUNZIONE GIANMARCO
## input
fb<-dataset[,"nuovo_fattore"]
dataset=dataset[order(dataset$nuovo_fattore),]
y<-dataset[,dep]
## controlli
if ( !is.factor(fb) )
stop("Il primo vettore colonna della matrice dei dati non Ë un fattore!")
## calcoli
vetcm<-rep((as.numeric(summary(fb))),(as.numeric(summary(fb))))
vetcc<-rep(cb,(as.numeric(summary(fb))))
num<-t(y)%*%(vetcc*vetcm^(-1))
mod<-summary(aov(dataset[,dep]~nuovo_fattore, dataset))
varerr<-mod[[1]][2,3]
df.varerr<-mod[[1]][2,1]
correz<- ((as.numeric(summary(fb)))^(-1)) %*% ((cb)^2)
den<-sqrt(varerr*correz)
tval<-num/den
pval<-(1-pt(abs(tval),df.varerr))*2
## ouput
cat("\n","\n")
cat(substr(paste("****************** Risultati ",dep, "~",paste(bet, sep="_")," ********************************"),1,62),"\n","\n")
cat("Contrasto inserito:    ",cb,"\n")
cat("Media del contrasto =",num,"\n")
cat("Media del Primo Gruppo =",MediaGruppo1,"\n")
cat("Media del Secondo Gruppo =",MediaGruppo2,"\n")
cat("Primo gruppo between:   ",sort(paste(round(cb[cb>0],2),names(tabella)[cb>0], sep=" "),decreasing=TRUE),"\n")
cat("Secondo gruppo between:   ",sort(paste(round(cb[cb<0],2),names(tabella)[cb<0], sep=" "),decreasing=TRUE),"\n")
cat("t =",tval,"     df = ",df.varerr,"     p =",signif(pval,4),"(due code)","\n","\n")
cat("**************************************************************","\n","\n")
}
