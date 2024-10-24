# create two variables (artificially they are slightly correlated
education = rnorm(100, 10, 10) 
CRI = rnorm(100, 80, 20) + education + rnorm(100, sd=5)

# inspect this artificial correlation
plot(CRI, education)
cor.test(CRI, education)

# create RT as a variables that depends on CRI and education plus some noise
noise = rnorm(100, sd=100)
RT = 2* education + 3*CRI + noise

# create a dataframe.
dat = data.frame(RT=RT, CRI=CRI, education=education)
# plot all correlations
pairs(dat)

# first model. RT depends on CRI and education. But CRI include some intrinsic correlation with education
mod1 = lm(RT~CRI+education, dat)
summary(mod1)


# I create a new variable in which I remove the (linear) effect of education on CRI
# this is called "residualized" the variable (as I calculate the residuals from a linear model)
CRI_noed = residuals(lm(CRI~education, dat))

dat[, "CRI_noed"] = CRI_noed
plot(CRI_noed, education)
cor.test(CRI_noed, education)
# now the new variable is no more correlated with education.
# the CRI_noed is a variable that may be conceived as the CRI, without the linuear effect of education.


# now I can calculate the effect of education and CRI WITHOUT the linear componenet of education
mod2 = lm(RT~CRI_noed+education, dat)
summary(mod2)

