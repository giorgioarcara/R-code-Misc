#### PROVA MISTO "FATTO BENE"

################################
## IMPORTAZIONE E PREPARAZIONE DATASET
################################
setwd("/Users/giorgioarcara/Documents/Documenti/Dottorato/paper TIME vs EVENT/ANALISI_PM_TIME/comportamentali")
dat=read.table("PMTIME_sogg_1-16.txt", header=TRUE, sep="\t")

######### PASSI PRELIMINARI SUI NOMI E SELEZIONE COLONNE ######
# questa serie di passaggi serve per avere in seguito corrispondenze nel numero di soggetti per i comportamentali e i dati ERP.
library(languageR)

### dati su monitoraggi
giovani.Subject=c(1:15)
Nmonitor.giovani=c(25, 4, 9, 8, 7, 8, 24, 22, 39, 13, 40, 28, 10, 36, 21)

monitors=data.frame(Nmonitors=Nmonitor.giovani, Subject=giovani.Subject)
monitors$Subject=factor(monitors$Subject)
###################

levels(dat$Name)=c("Salvo", "Cotroneo", "Guolo", "Nuovo1","Nuovo1","Marinato","Libri", "Colosio", "Busetto", "Darrer", "Nuovo2","Elbe","Libri", "Furlan", "Ghezzer", "Rabini", "Fumai", "Genovese")
dat$Name=as.character(dat$Name)
nomi=dat[,c("Name", "Subject")]
nomi=unique(nomi)
nomi=nomi[order(nomi$Name),]
nomi$Subject=as.factor(1:dim(nomi)[1])

dat0=dat
## seleziono le colonne che mi interessano
dat=dat[,c(9,34,36,37,39,42,46,52,41, 43)]
dat=merge(dat, nomi, by.x="Name", by.y="Name", all.x=TRUE)

######################################

############## rinomino alcune variabili
names(dat)[c(1,5,7,8)]=c("Subject.name","condition","ACC", "RT")
levels(dat$condition)=c("practice", "practice", "PM", "baseline")
dat$Subject=as.factor(dat$Subject)


dat=dat[!is.na(dat$RT)&dat$condition!="practice",]


dat=dat[dat$ACC==1,]


### COMBINO DAT E MONITOR #####
datprac=merge(dat, monitors, by.x="Subject", by.y="Subject", all.x=TRUE, all.y=F)



### CREO IL DATAFRAME PER MISTI

datprac$NumTrial[datprac$condition=="PM"]=dat$NumTrial[datprac$condition=="PM"]+40
datpracACC=datprac[datprac$NumTrial<400,]

#questo devo metterlo dopo altrimenti mi filtra già per le accurate.
datprac=datprac[datprac$ACC==1&datprac$NumTrial<400,]

datprac=datprac[,c("RT", "Subject", "condition", "SameDiff","NumTrial", "Stimulus", "Nmonitors")]




#faccio lo stesso usando la funzione di Baayen
datprac$RTprec1=lags.fnc(datprac, time="NumTrial", group = "Subject", depvar = "RT", lag=1)
datprac$RTprec2=lags.fnc(datprac, time="NumTrial", group = "Subject", depvar = "RT", lag=2)
datprac$RTprec3=lags.fnc(datprac, time="NumTrial", group = "Subject", depvar = "RT", lag=3)
datprac$RTprec4=lags.fnc(datprac, time="NumTrial", group = "Subject", depvar = "RT", lag=4)

#siccome  RTprec1, RTprec2, RTprec 3 sono correlate utilizzo il metodo di Baayen, Wurm, & Ayecock (2007): faccio una PCA su queste variabili. e utilizzo le componenti. Altrimenti il modello si incasina.

cor(datprac[,c("RTprec1", "RTprec2", "RTprec3", "RTprec4")])
PC=prcomp(datprac[,c("RTprec1", "RTprec2", "RTprec3", "RTprec4")], scale=T)
props=(PC$sdev^2)/sum(PC$sdev^2)

# le prime 3 variabili mi spiegano l'84 % della varianza dei 4 trial precedenti
sum(props[1:3])

datprac$PC1=PC$x[,1]
datprac$PC2=PC$x[,2]
datprac$PC3=PC$x[,3]



datprac$cNumTrial=scale(datprac$NumTrial)



###### 
library(languageR)
qqmath(~log(RT)|Subject, datprac)

## provo la trasf logaritmica
qqnorm(log(datprac$RT))
qqline(log(datprac$RT))

# provo la transf inversa
qqnorm(1/datprac$RT)
qqline(1/datprac$RT)


datprac$RTinv=1/datprac$RT
datprac$RTprec1inv=1/datprac$RTprec1
datprac$RTprec2inv=1/datprac$RTprec2



# la transf inversa sembra migliore, ma tolgo anche qualche outlier
datpractrim=datprac[datprac$RTinv<0.003,]
# percentuale di dati tolti
((dim(datprac)[1])-(dim(datpractrim)[1]))/(dim(datprac)[1])*100


qqnorm(datpractrim$RTinv)
qqline(datpractrim$RTinv)

library(lme4)

datprac.lmer=lmer(-1000*RTinv~condition+SameDiff+cNumTrial+(1|Subject), datpractrim)
datprac.lmer1=lmer(-1000*RTinv~condition+SameDiff+cNumTrial+(1|Subject)+(1|Stimulus), datpractrim)
datprac.lmer2=lmer(-1000*RTinv~condition+SameDiff+cNumTrial+(1+condition|Subject)+(1+cNumTrial|Subject)+(1|Stimulus), datpractrim)
datprac.lmer3=lmer(-1000*RTinv~condition+SameDiff+cNumTrial+RTprec1inv+(0+condition|Subject)+(1+cNumTrial|Subject)+(1|Stimulus), datpractrim)


datprac.lmer4= lmer(-1000*RTinv~condition+SameDiff*condition+cNumTrial+RTprec1inv+(0+condition|Subject)+(1+cNumTrial|Subject)+(1|Stimulus), datpractrim)


datprac.lmermon= lmer(-1000*RTinv~condition+SameDiff*condition+cNumTrial+RTprec1inv+Nmonitors+(0+condition|Subject)+(1+cNumTrial|Subject)+(1|Stimulus), datpractrim)


#### con PC sui trial precedenti

datprac.lmer5= lmer(-1000*RTinv~condition+SameDiff*condition+cNumTrial+PC1+PC2+PC3+(0+condition|Subject)+(1+cNumTrial|Subject)+(1|Stimulus), datpractrim)

#confronto autocorrelazione dat.praclmer5 e datprac.lmer3
par(mfrow=c(1,2))
pacf(resid(datprac.lmer3))
pacf(resid(datprac.lmer5))
#nota che i due modelli non sono nested, quindi non posso confrontarli con anova. Ma potrei confrontarli con 
# BIC ed AIC vanno in direzioni diverse quindi forse manterrei il modello dat.lmer3, che è più semplice.
# ESISTE UN MODO PER VALUTARE L'IMPATTO NEGATIVO DI AUTOCORRELAZIONE CHE NON SI E' RIUSCITI A RIMUOVERE?.



