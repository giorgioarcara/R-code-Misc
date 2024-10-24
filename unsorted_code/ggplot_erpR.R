library(erpR)
library(ggplot2)

data(ERPsets)

word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)
time = pointstomsec(1:dim(word)[[1]], startmsec=-500, endmsec=1500, lengthsegment = dim(word)[[1]])

ggplot(word, aes(time, FPZ))+
  geom_line(size=1)+
  #theme_bw()+
  theme(panel.grid.major=element_line(color="lightgray"), 
        panel.background = element_blank())


