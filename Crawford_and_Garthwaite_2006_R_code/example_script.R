rm(list=ls())

# load some data 
load("example_data.RData")

# source che function (the example is for multiple regression)
source("CGcut.off.multi.R")
source("CGcut.off.multi_table.R")

# see the data
head(raw_data)


# develop a linear model. In developing the model note that all dummy variables must be explicitely codified as such.
# and that polynomial terms have to be specified as spearate variables. Also if you want to include polynomial terms, they should be added
# after the linear ones (as in the example below.)

model = lm(Percentuali~scolarita + sesso_dummy + scolarita_2, data=raw_data)
summary(model)

# a couple of notes
# if you have a factor you should specify it with dummy coding (i.e., 0,1)
# if you have polinomial terms (as ^2), you should create a new variable and fit the model
# with this new variable.


# EXAMPLE 1: calculate if a specific subject is below cut-off (useful in clinical setting)
# suppose you have a female participant with 18 years of education which obtain a value of 2 in my test
# my model (see summary(model))  has only education, the education square and the sex (0=female, y=male)
# then I create the vector like this

preds = c(18, 0, 18^2) 
myYobs = 2


# EXAMPLE 2: calculate the critical values for a given combination of predictor variables (useful for creating cut-off tables)
# I use the same values of before, but I leave Yobs to null
preds = c(18, 0, 18^2) 

# you can find specification on the argument of the function within the function itself (as comments)
# Note that I need to specify the columns n the raw_data used as referene, with the columns in the very same order of the predictors in model
CGcut.off.multi(controls_data=raw_data[, c("scolarita", "sesso_dummy", "scolarita_2")], model=model, pred=preds, Yobs = NULL , p.crit=0.05)

# the Y_ob_crit, is the value below which the observed values are to be considered below cut-off
# if you use the function within a loop with several combination of values of the preds (e.g. of education, age, and gender)
# you can build your value with cut-off. Typically I tend to round the observed value before.
# check the functio CGcut.off.multi_table, for a wrapper of this.
# Note that, to make this function works, you need the polynomial terms to be after all the linear terms. SO the original linear model should be developed in this sense.
# note also that I do not insert polynimial in the pred_list, but on a separate list.

CGcut.off.multi_table(controls_data = raw_data[, c("scolarita", "sesso_dummy", "scolarita_2")], preds_list = list(scolarita=seq(5, 20, 5), Sex=c(0,1)), poly_list = list(c("scolarita", 2)), model = model, p.crit = 0.05)







