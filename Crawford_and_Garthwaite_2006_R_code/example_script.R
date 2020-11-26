rm(list=ls())

# load some data 
load("example_data.RData")

# source che function (the example is for multiple regression)
source("CGcut.off.multi.R")

# see the model
head(controls_data)

# see the model
summary(model)

# a couple of notes
# if you have a factor you should specify it with dummy coding (i.e., 0,1)
# if you have polinomial terms (as ^2), you should create a new variable and fit the model
# with this new variable.


# EXAMPLE 1: calculate if a specific subject is below cut-off (useful in clinical setting)
# suppose you have a female participant with 18 years of education which obtain a value of 2 in my test
# my model (see summary(model))  has only education, the education square and the sex (0=female, y=male)
# then I create the vector like this

preds = c(18, 18^2, 0) 
myYobs = 2

# you can find specification on the argument of the function within the function itself (as comments)
CGcut.off(controls_data=controls_data, model=model, pred=preds, Yobs = myYobs , p.crit=0.025)
 

# EXAMPLE 2: calculate the critical values for a given combination of predictor variables (useful for creating cut-off tables)
# I use the same values of before, but I leave Yobs to null
preds = c(18, 18^2, 0) 

# you can find specification on the argument of the function within the function itself (as comments)
CGcut.off.multi(controls_data=controls_data, model=model, pred=preds, Yobs = NULL , p.crit=0.05)

# the Y_ob_crit, is the value below which the observed values are to be considered below cut-off
# if you use the function within a loop with several combination of values of the preds (e.g. of education, age, and gender)
# you can build your value with cut-off. Typically I tend to round the observed value before.
# check the functio CGcut.off.multi_table, for a wrapper of this.








