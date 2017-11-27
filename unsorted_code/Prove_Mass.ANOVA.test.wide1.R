#####################
# LOOP MASS ANOVA #
#####################

### the logic of the script
# - first convert the erplist to wide format (one column for each timepoint x electrode). one row for each combination subject x condition
#  - second use sapply to perform t-tests with a fixed factor structure to several columns.
#  - to this I first split the data in two parts: the factor part (datall_vars, or datall_W_vars) and the "value" (dependent variables) part,
# named datall_value or datall_W_value. 
# - I then call a sapply() with a custom made function that will retrive the correct data using the summary output.



rm(list=ls())

library(erpR)
library(tictoc)
library(ez)

source("/Users/giorgioarcara/Documents/R_code_Giorgio/unsorted_code/erplist2long.R")
source("/Users/giorgioarcara/Documents/R_code_Giorgio/unsorted_code/erplist2wide.R")



data(ERPsets)



datall_word = erplist2long("Exp1_word_subj", 1:20, erplist = ERPsets, startmsec = -200, endmsec=1500, name.dep="Ampl")
datall_word$type="word"
datall_nonword = erplist2long("Exp1_nonword_subj", 1:20, erplist = ERPsets, startmsec = -200, endmsec=1500, name.dep="Ampl")
datall_nonword$type="nonword"
datall=rbind(datall_word, datall_nonword)


datall$Subject=as.factor(datall$Subject)
datall$type=as.factor(datall$type)


#########################
# EXPERIMENT WIDE FORMAT
#########################


datall_wordW = erplist2wide("Exp1_word_subj", 1:20, erplist = ERPsets, startmsec = -200, endmsec=1500, name.dep="Ampl")
datall_wordW$type="word"
datall_nonwordW = erplist2wide("Exp1_nonword_subj", 1:20, erplist = ERPsets, startmsec = -200, endmsec=1500, name.dep="Ampl")
datall_nonwordW$type="nonword"

datall_W = rbind(datall_wordW, datall_nonwordW)


datall_W_vars = datall_W[ c("Subject", "type")]
datall_W_values = datall_W[, -c(1,2, length(datall_W))] 
# I remove Subject, Subject_name and type, so i have only values


## this code is to understand the correct syntax for the ad-hoc retrievep function
aov.res = aov(Fp11~type + Error(Subject/type), data=datall_W)


#############################
## create a custom function to be fed to apply
#############################

retrievep=function(x){
  tryCatch(
  # try part
  {
  aov.res = aov(x ~ datall_W_vars$type + Error(datall_W_vars$Subject/datall_W_vars$type));
  temp = unlist(summary(aov.res))
  my_p = temp["Error: datall_W_vars$Subject:datall_W_vars$type.Pr(>F)1"]
  return(as.numeric(my_p))
  }, 
  # error part
   error = function(e){return(NA)}
  # do nothing for warning
  )
}


tic()
my_res = sapply(datall_W_values, retrievep)
toc()


res = mass.t.test("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, startmsec=-200, endmsec=1500, 
                  erplist1=ERPsets, erplist2=ERPsets, paired=T)

plot(unlist(res$p.mat)==my_res)
# they are basically the same even if (for a matter of rounding). Values are not exactly the same

all(round(unlist(res$p.mat, 2))==round(my_res, 2))

# you can check, they are basically identical.
anova_res = my_res
t_res = unlist(res$p.mat)
which(!anova_res==t_res)


#################################
##### PROVA CON 2 LIVELLI ######
#################################
load("/Users/giorgioarcara/Documents/Lavori Unipd/Progetto Mass Count/ERP qualche/Exp numerosity 2 ERP analysis/ExpNumerosity R analysis/R Data/Exp2Second.RData")


library(erpR)
library(tictoc)

source("/Users/giorgioarcara/Documents/R_code_Giorgio/unsorted_code/erplist2long.R")
source("/Users/giorgioarcara/Documents/R_code_Giorgio/unsorted_code/erplist2wide.R")



Subjects= c(1:4, 6:27)
my_startmsec = -500
my_endmsec = 1500

dat_m_alcuni_sg = erplist2wide("m_alcuni_sg_", Subjects, erplist = Exp2Second, startmsec = my_startmsec, endmsec=my_endmsec, name.dep="Ampl")
dat_m_alcuni_sg$type="alcuni"
dat_m_alcuni_sg$singpl="sg"
dat_m_alcuni_pl = erplist2wide("m_alcuni_pl_", Subjects, erplist = Exp2Second, startmsec = my_startmsec, endmsec=my_endmsec, name.dep="Ampl")
dat_m_alcuni_pl$type="alcuni"
dat_m_alcuni_pl$singpl="pl"
dat_m_un_sg = erplist2wide("m_un_sg_", Subjects, erplist = Exp2Second, startmsec = my_startmsec, endmsec=my_endmsec, name.dep="Ampl")
dat_m_un_sg$type="un"
dat_m_un_sg$singpl="sg"
dat_m_un_pl = erplist2wide("m_un_pl_", Subjects, erplist = Exp2Second, startmsec = my_startmsec, endmsec=my_endmsec, name.dep="Ampl")
dat_m_un_pl$type="un"
dat_m_un_pl$singpl="pl"

datall = rbind(dat_m_alcuni_sg, dat_m_alcuni_pl, dat_m_un_sg, dat_m_un_pl)
datall$type=factor(datall$type)
datall$singpl=factor(datall$singpl)


# code to build the ad-hoc retrivep() function

(aov.res = summary(aov(Fp11 ~ type*singpl + Error(Subject/(type*singpl)), datall)))
unlist(aov.res)

datall_values = datall[, -c(1, 2, length(datall)-1, length(datall))]
datall_vars =  datall[, c(1, 2, length(datall)-1, length(datall))]


retrievep2=function(x){
  tryCatch(
    # try part
    {
      aov.res = aov(x ~ datall_vars$type * datall_vars$singpl+Error(datall_vars$Subject/(datall_vars$type*datall_vars$singpl)));
      temp = unlist(summary(aov.res))
      my_p = matrix(NA, ncol=3)
      my_p[,1] = temp["Error: datall_vars$Subject:datall_vars$type.Pr(>F)1"]
      my_p[,2] = temp["Error: datall_vars$Subject:datall_vars$singpl.Pr(>F)1"]
      my_p[,3] = temp["Error: datall_vars$Subject:datall_vars$type:datall_vars$singpl.Pr(>F)1"]
      
      return(as.numeric(my_p))
    }, 
    # error part
    error = function(e){return(NA)}
    # do nothing for warning
  )
}




tic()
my_res = sapply(datall_values, retrievep2)
toc()



retrieveaov=function(x){
  tryCatch(
    # try part
    {
      aov.res = summary(aov(x ~ datall_vars$type * datall_vars$singpl+Error(datall_vars$Subject/(datall_vars$type*datall_vars$singpl))));
      return(aov.res)
    }, 
    # error part
    error = function(e){return(NA)}
    # do nothing for warning
  )
}



tic()
my_res = lapply(datall_values, retrieveaov)
toc()








