library(erpR)

source("/Users/giorgioarcara/Documents/R_code_Giorgio/unsorted_code/funzioni erpR altre/grandaverage.se.R")
source("/Users/giorgioarcara/Documents/R_code_Giorgio/unsorted_code/funzioni erpR altre/grandaverage.bands.R")


data(ERPsets)
# compute the average of subjects 1 to 20 for the condition 
# specified by the string "Exp1_word_subj".

word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)

word.se = grandaverage.se ("Exp1_word_subj", 1:20, erplist=ERPsets)

myband = grandaverage.bands(word, word.se, "FPZ")

library(ggplot2)


word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)
time = pointstomsec(1:dim(word)[[1]], startmsec=-500, endmsec=1500, lengthsegment = dim(word)[[1]])

### WITH GGPLOT

ggplot(word, aes(time, FPZ))+
  geom_line(size=1)+
  #theme_bw()+
  geom_ribbon(data = myband, aes(ymin=lower,ymax=upper), alpha=0.3, col="darkgray") +
  theme(panel.grid.major=element_line(color="lightgray"), 
        panel.background = element_blank())

### WITH BASE R
#
my_el="FPZ"

erp(word[,my_el])
polygon(x=c(1:length(word[,my_el]), length(word[, my_el]):1), y = c(myband$lower, rev(myband$upper)), col=rgb(0.8, 0.8, 0.8,0.5))
erp.add(word[, my_el], lwd=2) # NOTE! I re-plot the electrode over.



# check se are computed correctly
# check with a single point (lot of hard-coding)
my_vec=NULL
point_to_check=1

for (i in 1:20){
  my_vec[i]=ERPsets[[i]]$FCZ[1]
} # word

se = sd(my_vec)/sqrt(length(my_vec))
se


word.se$FCZ[point_to_check]





