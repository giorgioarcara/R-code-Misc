#######################
# Linear Mixed Models #
#######################


### Example lme() for linear mixed models in R ###
library(nlme)
?Orthodont

### plot the data ###
library(lattice)
xyplot(distance ~ age, Orthodont, groups = Subject, type = c('g','l')) #Spaghetti plot
plot(Orthodont)   #plot will use a special plot by default, as Orthodont is a special data set, "groupedData"

### Orthodont is "groupedData", thus lme will fit random intercept and slope by subject by default ###
lme(distance ~ age, data = Orthodont,method="ML") 
### We can define our own random effects ###
lme(distance ~ age + Sex, random = ~ 1, data = Orthodont) # just an intercept
lme(distance ~ age + Sex, random = list(Subject = ~ 1),data=Orthodont) #or like this, if not groupedData
lme(distance ~ age + Sex, random = list(Subject = ~ 1+age), data = Orthodont) # random intercept and slope


### Example mgcv: gamm() for additive mixed models in R ###
library(mgcv)
?Soybean
plot(Soybean)
Soy <- Soybean 
Soy$logweight <- log(Soy$weight)
xyplot(logweight ~ Time, Soy, groups = Plot, type = c('g','l')) #Spaghetti plot

g1 <- gamm(logweight ~ Year + Variety +  s(Time, bs = "ps", m = c(2,1), k = 20),random = list(Plot = ~ 1 + Time), data = Soy, method = "REML") # allow for an overall smooth function in time (same shape for all plots),
# but allow intercept and slope to differ by plot (randomly)
g1
plot(g1$gam)

g2 <- gamm(logweight ~ Year + Variety +  s(Time, bs = "ps", m = c(2,1), k = 20),random = list(Plot = ~ 1), data = Soy, method = "REML")  # allow for an overall smooth function in time (same shape for all plots),
# but allow the intercept to differ by plot (randomly)
g2
plot(g2$gam)


### Example testing for a variance component / random effect ###
library(nlme)
lme1 <- lme(distance ~ age + Sex, random = list(Subject = ~ 1),data=Orthodont) #random intercept
lme2 <- lme(distance ~ age + Sex, random = list(Subject = ~ 1+age), data = Orthodont) #random intercept + slope
### Is the random slope necessary? Variance small compared to variance of random intercept ###
test.statistik <- summary(lme2)$logLik - summary(lme1)$logLik
test.statistik
p.value <- 0.5*(1-pchisq(test.statistik,1))+0.5*(1-pchisq(test.statistik,2)) 
p.value  #p-value from equal mixture chi_1^2:chi_2^2





###################################
# Generalized Linear Mixed Models #
###################################


### Example simple generalized linear mixed model with lme4 and mgcv ###
library(lme4); library(mgcv)
### simulate Poisson data with random intercept###
I <- 100
n.i <- 8
subject <- rep(1:I,each=n.i)
random.intercept <- rep(rnorm(I),each=n.i)
time <- rep(1:n.i,I)
beta.0 <- beta.1 <- 1
lambda <- exp(beta.0 + random.intercept + beta.1*time)
Y <- rpois(I*n.i,lambda)
### fit a model ###
glmer(Y ~ time + (1 | subject), family = poisson)         #Laplacian approximation
glmer(Y ~ time + (1 | subject), family = poisson,nAGQ=20) #Adaptive Gaussian Quadrature (L=20) gives same result here
gamm(Y ~ time, random = list(subject = ~ 1), family = poisson)  #Similar result using PQL 
# However: log-likelihood value very different, as log-likelihood for the linear mixed model!

### speed comparison ###
system.time(glmer(Y ~ time + (1 | subject), family = poisson))        
system.time(glmer(Y ~ time + (1 | subject), family = poisson,nAGQ=20)) #not much slower in this simple example
system.time(gamm(Y ~ time, random = list(subject = ~ 1), family = poisson))  #slower 



### Seizure data discussed also in Example 8.5 / 9.5 in Diggle, Heagerty, Liang & Zeger (2002) ###
library(geepack)
data(seizure)
?seizure
### Reshape data set, one line per subject and time point ###
seiz.l <- reshape(seizure, varying=list(c("base","y1", "y2", "y3", "y4")), v.names="y", times=0:4, direction="long")
seiz.l <- seiz.l[order(seiz.l$id, seiz.l$time),]
seiz.l$t <- ifelse(seiz.l$time == 0, 8, 2)  #length of time period (8 weeks for baseline, 2 weeks else)
seiz.l$x <- ifelse(seiz.l$time == 0, 0, 1)  #mark baseline

### fit generalized linear mixed model ###
### parameter of interest: beta for x:trt, the interaction between baseline and treatment ###
### - is the change since baseline different in the two treatment groups? ###
### correct for different lengths of time periods with an offset, log(t) ###
### allow for individual differences in seizure rates (random intercept) ###
m1 <- glmer(y ~ offset(log(t)) + x + trt + x:trt + (1 | id), family = poisson,nAGQ=20, data=seiz.l) 
m2 <- glmer(y ~ offset(log(t)) + x + trt + x:trt + (1+x | id), family = poisson,nAGQ=20, data=seiz.l)
m1
m2 #there might be heterogeneity between patients also in the change in seizure rate

LRT <- logLik(m2)-logLik(m1)
LRT  #yes

