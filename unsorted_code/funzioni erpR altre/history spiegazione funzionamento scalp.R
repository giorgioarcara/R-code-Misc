library(erpR)

data(ERPsets)

word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)

# nell'oggetto my array stabilisco il numero di righe e di colonne dello scalp
myarray=c(7,5)

# creo un vettore con gli elettrodi e in pià 
# - "yaxis", dove voglio mettere l'asse con le ampiezze y
# - "xaxis" dove voglio l'asse col tempo
# - "blank" dove voglio del vuoto.

myelectrodes = c("yaxis", "Fp1", "blank", "Fp2", "legend", 
               "F7", "F3", "FZ", "F4", "F8", "FT7", "FC3", "FCZ", 
               "FC4", "FT8", "blank", "C3", "CZ", "C4", "T4", "TP7", 
               "CP3", "CPZ", "CP4", "TP8", "T5", "P3", "PZ", "P4", 
               "T6", "xaxis", "O1", "OZ", "O2", "blank")

scalp(list(word), scalp.array=myarray, layout=myelectrodes, ylim=12)


# adesso ne creo uno diverso fatto da quattro elettrodi
# con una colonna bianca in mezzo
myarray2=c(2,3)
myelectrodes2=c("Fp1", "blank", "Fp2", "P3", "blank",  "P4")

scalp(list(word), scalp.array=myarray2, layout=myelectrodes2, ylim=12)


#scalp.t("Exp1_word_subj", "Exp1_word_subj", numbers=1:20, numbers2=1:20, erplist1=ERPsets, erplist2=ERPsets, scalp.array=myarray, layout=myelectrodes2, ylim=12)


source("C:/Users/Mamma/Desktop/nuovi files/R files/scalp.t.new.R")
source("C:/Users/Mamma/Desktop/nuovi files/R files/mass.t.test.R")

scalp.t("Exp1_word_subj", "Exp1_word_subj", numbers=1:20, numbers2=1:20, erplist1=ERPsets, erplist2=ERPsets, scalp.array=myarray, layout=myelectrodes, ylim=12)

