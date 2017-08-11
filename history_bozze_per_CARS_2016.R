library(erpR)

data(ERPsets)


# devi caricare diverse funzioni
# stable.diff.t.test, plot.raster mass.t.test2


stab.res=stable.diff.t.test("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, startmsec=-200, endmsec=1500, paired=T, erplist1=ERPsets, crit.msec=60)

mass.res=mass.t.test2("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, startmsec=-200, endmsec=1500, paired=T, erplist1=ERPsets, interval=c(0,600), p.adj="fdr")
plot.raster(mass.res$mass.t.results, startmsec=-200, endmsec=1500)


mass.res=mass.t.test2("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, startmsec=-200, endmsec=1500, paired=T, erplist1=ERPsets, interval=c(0,600), p.adj="fdr", electrodes = c("F3", "F4", "C3", "C4"))
plot.raster(mass.res$t.mat, startmsec=-200, endmsec=1500)




