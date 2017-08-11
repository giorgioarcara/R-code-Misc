### PROVE MIXED MODEL ####

setwd("~/Documents/Documenti/Dottorato/lavori per altri/")

dat=read.table("Francesca Franzon/24_part_nuovo(1).txt", sep="\t", quote="", header=T)

library(lme4)


### MODELLO CON ITEMS, SUBJECTS e 1 FIXED FACTOR
dat1=with(dat[dat$ACC==1,], aggregate(RT, list(TipoDiGenere, Item, Subject), mean))
names(dat1)=c("TipoDiGenere", "Item", "Subject", "RT")


dat1.lmer=lmer(RT~TipoDiGenere+(1+TipoDiGenere|Subject)+(1|Item), dat1)

dat1.lmer1=lmer(RT~TipoDiGenere+(1+TipoDiGenere|Subject)+(1|Item)+(1|Subject), dat1)
# non ha molto senso. Se sommi le colonne dei ranef di subject ottieni lo stesso aggiustamento dell'intercetta di quello sopra.
# lo stesso per la random slope che è pressoché identica. ()
# (prova a confrontare ranef(dat1.lmer)[[2]][2], ranef(dat1.lmer1)[[2]][3])


dat1.lmer2=lmer(RT~TipoDiGenere+(0+TipoDiGenere|Subject)+(1|Item)+(1|Subject), dat1) # questo modello fa un aggiustamento a slope e intercept, assumendo indipendenza


dat1.lmer3=lmer(RT~TipoDiGenere+(0+TipoDiGenere|Subject)+(1|Item), dat1) # questo modello fa un aggiustamento solo alla slope, non alla Intercept


# MODELLO CON ITEMS, SUBJECTS, 1 FIXED FACTOR, 1 FATTORE LISTA