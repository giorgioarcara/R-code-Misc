x1 = rnorm(100)
x2 = x1 + rnorm(100, sd = 0.5)
x3 = x1 + rnorm(100, mean=100, sd = 0.5)

dat1 = data.frame(x1=x1, x2=x2, x3=x3)


y1 = rnorm(100)
y2 = y1 + rnorm(100, sd = 0.5)
y3 = y1 + rnorm(100, sd = 0.5)

dat2 = data.frame(y1=y1, y2=y2, y3=y3)

dat = cbind(dat1, dat2)

###################################
##### PRELIMINARY CORRELATIONS ####
###################################
cor(dat)

library(corrplot)

corrplot(cordat, order = "AOE")

###############
##### PCA ####
################

dat.pca = prcomp(dat, scale=TRUE)

x = scale(as.matrix(dat))%*%dat.pca$rotation 

round(dat.pca$x,2)==round(x,2)

##alternative (be careful)
dat.pca2 = princomp(dat)

names(dat.pca)


#######################################
### STANDARD DEVIATION ###############
#######################################

# contains standard deviations (used to calculate % explained variance)
dat.pca$sdev

props=round( (dat.pca$sdev^2/sum(dat.pca$sdev^2)) , 3)

barplot(props, col=props>0.05)
# or
screeplot(dat.pca)

#######################################
# PRINCIPAL COMPONENT SCORES
#######################################
# contaoins principal component scores
dat.pca$x

corrplot( cor(dat.pca$x) ) 

# which components to retain?
# 1) components that explain at least x% of variance (e.g., 5%)
# 2) components till the first discontinuity


dat.pca$rotation

# M * V = X
# dati * rotation = principal components

###
# USAGES #
############

# 1) reduce number of dependent variables if they are correlated (simplify)
# 2) reduce collinearity between predictors in regressions.
# 3) 

###################################
# HOW TO DECORRELATE VARIABLES ###
##################################
x1 = rnorm(100) # es. Parent's Education 
x2 = x1 + rnorm(100, sd = 0.1) #  Parent's Education + QI

y1 = x1 + x2 + rnorm(1000, sd=1) # last term in this case indicates noise


dat=data.frame(x1=x1, x2=x2, y1=y1)

cor(dat)

corrplot(cor(dat))

dat.lm = lm(y1 ~ x1 + x2, dat)

summary(dat.lm)

vif(dat.lm)


res.lm = lm(x2 ~ x1, dat)

# add residualized score
dat$x2.res = residuals(res.lm)

summary(res.lm)

##########
dat.lm = lm(y1 ~ x1 + x2.res, dat)

#####################
# REGRESSION TREES
###################

x1 = rnorm(100) # es. Parent's Education 
x2 = x1 + rnorm(100, sd = 0.1) #  Parent's Education + QI

y1 = x1 + x2 + rnorm(1000, sd=1) # last term in this case indicates noise


library(rpart)

dat.lm  = lm(y1 ~ x1 + x2, dat)
dat.rpart = rpart(y1 ~ x1 + x2, dat)

plot(dat.rpart)
text(dat.rpart)



### MEAN SQUARED ERROR ###
mse.rpart = mean( resid(dat.rpart)^2 )
mse.lm = mean(resid(dat.lm)^2)


# test: if TRUE go to the left, if false to the right

# TREE PRUNING
# check the first cp below the dashed line
plotcp(dat.rpart)

# prune the tree (here simulated)
dat.rpart2 = prune(dat.rpart, cp=0.25)

plot(dat.rpart2)
text(dat.rpart2)

mse.rpart2 = mean( resid(dat.rpart2)^2 )
