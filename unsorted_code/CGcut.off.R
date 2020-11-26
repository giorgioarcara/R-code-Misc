
setwd("/Users/giorgioarcara/Documents/Lavori San Camillo/NADL-F/NADL - F taratura/")
source('history NADL-F regressioni per cut-offs.R', chdir = TRUE)

# create data: a matrix with cases x vars (only those)
controls_data = cbind(dat.con$scolarita, dat.con$scolarita_2, dat.con$sesso_dummy)

# lm model
model=Percentuali.lm.co
pred_names = names(model$coefficients)[-1] # -1 is to remove the intercept
# sigma from the lm model
# number of subjects

# my desidred prediction (pred values)
preds = c(18, 324, 1)

Yobs = 3

CGcut.off(controls_data=controls_data, model=model, pred=preds, Yobs = 2, p.crit=0.025)

CGcut.off<-function(controls_data, model = NULL, preds = NULL, Yobs = NULL, p.crit=0.05){
  
  #######################
  # PERFORM SOME CHECKS #
  #######################
  if (is.null(model)){
    stop("you should specify a regression model (model), calculated on controls data")
  }
  
  if (is.null(controls_data)){
    stop("you should specify a the controls data, on which the model is calculated")
  }
  
  if (is.null(preds)){
    stop("you should include the values of the predictors")
  }
  
  if (length(preds)!=dim(controls_data)[2]){
    stop("the length of predictor values should be the same of the number of columns of controls_data")
  }
  
  
  
  sigma = summary(model)$sigma 
  
  # calculate n of observation of controls (i.e., numbers of control subjects)
  n = dim(controls_mat)[1]
  
  # calculate number of variables (i.e., number of predictors)  
  nvar=dim(controls_mat)[2] # numero variabili
  
  # calculate mean ov all variables
  vars_mean = apply(controls_mat, 2, mean)  
  
  rmat=cor(controls_mat) # correlation matrix
  rinv=solve(rmat) # l'inverted correlation matrix (needed for the la formula)
  rinv.diag=diag(rinv) # elementi nella diagonale.
  rinv.offdiag=rinv[upper.tri(rinv)]
  
  # loop to calculate needed k (variables)
  
  zio = rep(0, nvar)
  
  for (i in 1:nvar){
    zio[i] = ((n-1)*(preds[i]-vars_mean[i]))/sum((controls_mat[,i]-vars_mean[i])^2)
  }
  
  zio_2 = zio^2
  ## I call A and B, the first and second sum within Eq. 6
  # transform the Sum in matrix multiplication (I checked and it is correct)
  # A=zio_2[1]*rinv.diag[1]+zio_2[2]*rinv.diag[2]
  
  A=rinv.diag%*%zio_2 # this generalize to every length of zio_2
  
  # to obtain B, first I creat a mtrix with all possible multiplication between z
  # to to dhat I use the cross-product function %o%
  
  zio_2_multi=zio_2%o%zio_2
  
  # isolate off.diag elements
  zio_2_multi.offdiag=zio_2_multi[upper.tri(zio_2_multi)]
  
  B=rinv.offdiag%*%zio_2_multi.offdiag
  
  Sn_1=sigma*(sqrt( 1 + ( 1/n ) + ( 1/(n-1) ) * A + ( 2/(n-1) ) * B ))
  
  # find critical t and then the result
  newdata =  as.list(preds)
  names(newdata) = pred_names
  Y_pred=predict(model, newdata=newdata)
  df=(n-nvar-1)  # see fourth lines of p. 262 (C&G 2006)
  t.crit=qt(p=p.crit, df=df) # note I use 0.05
  
  # the following formula is formula (3) by Crawford and Garthwaite 2006, inverted. 
  # In the original formula, the ouput value is the observed t.
  Y_obs_crit=(t.crit*Sn_1)+Y_pred 
  
  t.obs = (Yobs - Y_pred)/Sn_1
  p.obs = pt(t.obs, df = df)
  
  ##################
  # create results #
  ##################
  
  if (!is.null(Yobs)){
    res = data.frame(t.obs=t.obs,  df = df, p.obs = p.obs, t.crit=t.crit, p.crit=p.crit)
  } 
  
  if (is.null(Yobs)){
    res = data.frame(t.crit = t.crit, df=df, p.crit=p.crit)
  }
  return(res)
}

