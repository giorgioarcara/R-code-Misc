#################################
##### PROVA CON 1 LIVELLO ######
#################################
rm(list=ls())


load("/Users/giorgioarcara/Documents/Lavori Unipd/Progetto Mass Count/ERP qualche/Exp numerosity 2 ERP analysis/ExpNumerosity R analysis/R Data/Exp2Second.RData")

source("/Users/giorgioarcara/Documents/R_code_Giorgio/unsorted_code/erplist2long.R")


library(erpR)
data(ERPsets)



datall_word = erplist2long("Exp1_word_subj", 1:20, erplist = ERPsets, startmsec = -200, endmsec=1500, name.dep="Ampl")
datall_word$type="word"
datall_nonword = erplist2long("Exp1_nonword_subj", 1:20, erplist = ERPsets, startmsec = -200, endmsec=1500, name.dep="Ampl")
datall_nonword$type="nonword"
datall=rbind(datall_word, datall_nonword)


#https://github.com/craddm/EEG_Workshop/blob/master/exercises/Exercise_4_mass_univariate.Rmd

# nest the dataset by timepoints
library(tidyverse)
library(broom)




time_el_nest = nest(datall, -timepoints, -electrode)

tic()
time_el_nest <- mutate(time_el_nest,
                       stats = map(data, ~aov(Ampl ~ type+Error(Subject/type),
                                                  data = .x)))
toc()

# NOTE: approximately the same time of apply.




#################################
##### PROVA CON 2 LIVELLI ######
#################################
load("/Users/giorgioarcara/Documents/Lavori Unipd/Progetto Mass Count/ERP qualche/Exp numerosity 2 ERP analysis/ExpNumerosity R analysis/R Data/Exp2Second.RData")


library(erpR)
library(tictoc)
library(tidyverse)
library(broom)

source("/Users/giorgioarcara/Documents/R_code_Giorgio/unsorted_code/erplist2long.R")
source("/Users/giorgioarcara/Documents/R_code_Giorgio/unsorted_code/erplist2long.R")



Subjects= c(1:4, 6:27)
my_startmsec = -500
my_endmsec = 1500

dat_m_alcuni_sg = erplist2long("m_alcuni_sg_", Subjects, erplist = Exp2Second, startmsec = my_startmsec, endmsec=my_endmsec, name.dep="Ampl")
dat_m_alcuni_sg$type="alcuni"
dat_m_alcuni_sg$singpl="sg"
dat_m_alcuni_pl = erplist2long("m_alcuni_pl_", Subjects, erplist = Exp2Second, startmsec = my_startmsec, endmsec=my_endmsec, name.dep="Ampl")
dat_m_alcuni_pl$type="alcuni"
dat_m_alcuni_pl$singpl="pl"
dat_m_un_sg = erplist2long("m_un_sg_", Subjects, erplist = Exp2Second, startmsec = my_startmsec, endmsec=my_endmsec, name.dep="Ampl")
dat_m_un_sg$type="un"
dat_m_un_sg$singpl="sg"
dat_m_un_pl = erplist2long("m_un_pl_", Subjects, erplist = Exp2Second, startmsec = my_startmsec, endmsec=my_endmsec, name.dep="Ampl")
dat_m_un_pl$type="un"
dat_m_un_pl$singpl="pl"

datall = rbind(dat_m_alcuni_sg, dat_m_alcuni_pl, dat_m_un_sg, dat_m_un_pl)
datall$type=factor(datall$type)
datall$singpl=factor(datall$singpl)


time_el_nest = nest(datall, -timepoints, -electrode)

tic()
time_el_nest <- mutate(time_el_nest,
                       stats = map(data, ~summary(aov(Ampl ~ type*singpl+Error(Subject/(type*singpl)),
                                              data = .x))))
toc()

####################
### TO UNLIST ######
####################
time_el_nest <- mutate (time_el_nest, res = map(stats, unlist))


pvals = map_dbl(time_el_nest$res, "Error: Subject:type.Pr(>F)1")
