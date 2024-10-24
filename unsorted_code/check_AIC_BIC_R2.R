#####
# STEP1: simulation with different data
#######

# set number of simulations
n_sim = 1000

# create empty object for results
res = data.frame(R2=rep(NA, n_sim), AIC=rep(NA, n_sim), BIC=rep(NA, n_sim))

for (iS in 1:n_sim){
  
  x = rnorm(100)
  y = x + rnorm(100, sd=2)
  mod = lm(y~x)
  
  res$R2[iS] = summary(mod)$r.squared
  res$AIC[iS] = AIC(mod)
  res$BIC[iS] = BIC(mod)

  
}

plot(res$AIC, res$BIC)



#####
# STEP2: simulation with same data, different transformation
#######
# launch this section several times.


# different transformations

cube = function(x){x^3}
quadr = function(x){x^2}
logm100 = function(x){log(100-x)}
log10m100 = function(x){log10(100-x)}
#log10mAve = function(x){log10(mean(x)-x)} # not included now.
inv = function(x){1/x}


transfs = c("identity", "cube", "quadr", "logm100", "log10m100",  "inv")

x = rnorm(100, mean=50)
y = x + rnorm(100, sd=2)

res2 = data.frame(R2=rep(NA, length(transfs)), AIC=rep(NA, length(transfs)), BIC=rep(NA, length(transfs)))

for (iT in 1:length(transfs)){
  
  mod = eval(parse(file="", text=paste("mod=lm(y~", transfs[iT], "(x) )", sep="")))
  
  res2$R2[iT] = summary(mod)$r.squared
  res2$AIC[iT] = AIC(mod)
  res2$BIC[iT] = BIC(mod)
  
  
}

plot(res2$R2, res2$AIC)



