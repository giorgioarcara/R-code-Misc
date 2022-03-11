v1 = rnorm(100)
v2 = v1 + rnorm(100)
v3 = v2 + rnorm(100)
v4 = rnorm(100)
v5 = rnorm(100)


# create a data.frame that contains only the variables relevant for PCA
dat = data.frame(v1, v2, v3, v4, v5)

# PCA cannot handle NA
dat = na.omit(dat)


# uncomment these lines to see the impact of high values in a variable with scale=F
#dat$v1=dat$v1*100
#dat = data.frame(v1,v2,v3)

#####################
# first step (see patterns of PAIRWISE correlation)
#####################
pairs(dat)
library(corrplot)
corrplot(cor(dat)) # see helpers 
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
library(languageR)
pairscor.fnc(data = dat)

# this step tells you if it is meaningful to perform PCA

#####################
# second step RUN PCA
#####################

# google for recommendation n >> p (nrow >> ncol)
dim(dat)

# Better always use scale (i.e., standardize variables as z-scoress, otherwise variables with higher values will dominate the results)
dat.pca=prcomp(dat, scale=T, center=T)
summary(dat.pca)

# PCA gives you tthree main results
# 1) rotation matrix with loadings
dat.pca.loadings=dat.pca$rotation

# 2) Principal Component Scores
dat.pca.compscores=dat.pca$x

# 3) percentage of variance of each component
props=round((dat.pca$sdev^2/sum(dat.pca$sdev^2)),3)


# how to select component to retain
# 1) select a priori some desidere variances
# for example I select variables summing up to 90%

# 2) at least 5%
barplot(props, col=props>0.05)
abline(h=0.05)

# 3) scree plot (search for discontinuities) (qualittative)
screeplot(dat.pca)

# 4) there are many other methods. (google)

#### HOW TO USE PCA RESULTS

# 1) INTERPRET LOADINGS
# check only loadings to interpret the pattern of correlation
print(dat.pca.loadings)

# 2) USE PRINCIPAL COMPONENT
# use PC as summary score of other variables.


library(psych)
dat.fapoly = fa(dat, nfactors = 3)


