setwd("C:/Users/Mamma/Desktop/nuovi files/R files")


source("erp.fun.cycle.R")

library(erpR)
data(ERPsets)

datall=erp.fun.cycle(bases=c("Exp1_word_subj", "Exp1_nonword_subj"), numbers=1:20, erp.fun=erp.mean, conditions.names=c("word", "nonword"), erplist=ERPsets, intervals.var=TRUE, intervals=list(c(100, 200), c(300, 500)), startmsec=-200, endmsec=1500)


dat1=erp.mean("Exp1_word_subj", 1:20,win.ini = 100, win.end=200, startmsec=-200, endmsec=1500, erplist=ERPsets, others=c(condition="word", interval="100-200"))
dat2=erp.mean("Exp1_word_subj", 1:20,win.ini = 300, win.end=500, startmsec=-200, endmsec=1500, erplist=ERPsets, others=c(condition="word", interval="300-500"))
dat3=erp.mean("Exp1_nonword_subj", 1:20,win.ini = 100, win.end=200, startmsec=-200, endmsec=1500, erplist=ERPsets, others=c(condition="nonword", interval="100-200"))
dat4=erp.mean("Exp1_nonword_subj", 1:20,win.ini = 300, win.end=500, startmsec=-200, endmsec=1500, erplist=ERPsets, others=c(condition="nonword", interval="300-500"))


datall2=rbind(dat1, dat2, dat3, dat4)

all(datall==datall2)
