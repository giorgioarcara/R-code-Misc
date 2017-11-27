#### package.list
# questa history serve per reinstallare tutti i pacchetti R che utilizzi.

# languageR: baayen analisi linguaggio
# tolerance: per limiti di tolleranza
# sn: distribuzione skewed normal
# pROC: analisi ROC
# car: analisi varie
# lme4: modelli misti da Pinheiro & Bates, 2000
# ez: ottimo per analisi ANOVA
# rms: è il nuovo nome del pacchetto Design di Frank Harrel.
# sciplot: per grafici con CI. utile per analisi inziali con anova.
# psych: funzioni utili per psicologi
# rela: funzioni per analisi fattoriale
# MASS: serie di funzioni per analisi multivariata.
# mvoutliers: funzioni per detezione outliers multivariati.
# psychometric: funzioni utili per varie analisi tra cui Item-analysis
# elasticnet: contiene spca, per effettuare sparse pca.
# gdata: contiene read.xls
# eRm : rasch scaling
# ltm: pacchetto per item response theory
# rpanel: utile per GUI, tutte le funzioni che hai con gui lo utilizzano. (VEDI SOTTO "IMPORTANTE")
# scatterplot3d 
# rgl #per grafici 3d
# plotrix # per disegnare cerchi
# effects #per disegnare partial effects in regression.
# influence.ME # package per calcolare dati influenti in mixed models.
# bayesclust # cluster analysis bayesiana con p-value (poco chiaro)
# pvclust # cluster analysis con p-value tramite bootstrap
# xgobi ## grafici 3d interattivi in R, devi però installare separatamente xgobi
# devtools,## serve per caricare il pacchetto likert, che va scaricato tramite github.
# likert ## pacchetto con grafici carini per scale likert.
# rattle ### data mining in R. (ma in R 3.0 problems con la configurazione di GTk)
# pwr # calcolo potenza .
# akima #interpolations method (necessarie per mappe scalpo in erpR)
# rminer #data mining in R
# gamm4 # generalized additive mixed model referring to lme4 routines.
# Matrix #necessary for lme4, sometimes not automatically installed as dependency.
# cvTools #useful tools for cross validation
# gamair # it contains the data.frame brain, an example of book by Woods (2006) on gam.
# effects # plot partial effects of linear and mixed models
# xlsx # read and write xlsx fils
# data.table # for the data.table class, similar to data.frame, but allowing some fast operations (like grouping etc.) with a peculiar syntax.
# corrplot # useful to plot correlation in several ways.
# equivalence # for TOST test (by Lakens)
# CTT # include a function for Spearman Brown formula.
# caret # for RandomForests
# ggraph # useful fror graph (used for same plot with caret.)
# tictoc useful for tic toc in matlab style
# tidyverse: all packages of the tidverse

##############
## IMPORTANTE
##############
# per poter installare TCL/TK package, devi anche avere installato un altro package (http://cran.r-project.org/bin/macosx/tools/)


package.list=c("mvoutlier", "MASS", "psych","languageR", "tolerance", "sn", "pROC", "car", "lme4", "ez", "rms", "sciplot", "rela", "psychometric", "gdata", "eRm", "rpanel", "scatterplot3d", "rgl", "plotrix", "effects", "ltm", "influence.ME", "bayesclust", "pvclust", "rggobi", "gamm4", "rminer", "akima", "pwr", "devtools", "Matrix", "cvTools", "equivalence", "caret", "ggraph", "tictoc", "tidyverse")



# questa è la library, nota che cambierà a seconda del nome della versione in corso!. Devi quindi vedere esattamente come è chiamata la cartella e ricreare la dir di conseguenza.
## NOTA!!!:se installi una nuova versione di R devi cambiare il numero di versione nella Rlibrarydir di conseguenza.

Rlibrarydir="/Users/giorgioarcara/Library/R/3.0/library"

install.packages(package.list, dependencies=T)

