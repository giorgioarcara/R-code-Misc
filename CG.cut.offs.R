CG.cut.offs<-function(obs, mod, vars, values, upper=FALSE){
  
  # consistency checks
  # check if the number of predictors is consistent with vars.list and vars.value
  n_pred=length(coef(mod))-1 # 1 is the intercept
  if(length(vars)!=n_pred|length(values)!=n_pred){
    stop(paste("There is an inconsistency in the arguments you supplied. Please check:\n",
"'mod' has length: ", n_pred, "\n",
          "'vars.list' has length:, ", length(vars), "\n",
          "'vars.values' has length: ", length(values), "\n", sep=""))
  }
  
  
  
  # case 1 predictor
  if (length(vars==1)){
    
    ## PREPARE SOME VARIABLES
    # get data.frame used in lm
    mod_data = mod$model
    
    # change order of columns for consistency
    # with vars
    mydata = mod_data[, vars]

    #get numerosity
    n = length(mydata)
    
    # get sigma
    sigma = summary(mod)$sigma
    
    # alternative way to calculate sigma. According to formula (1) in C & G paper
    # it yields virtually identifical results to the sigma from linear models.
    # dep=mod_data[, 1]
    # sigma=sd(dep)*sqrt((1-summary(mod)$r.squared)*( (n-1) / (n-2)))

    
    # calculate vars variances
    variances=var(mydata)
    # calvulcate vars means
    means=mean(mydata)
    
    
    Sn_1=sigma*(sqrt( 1 + ( 1/n ) + ( (values-means)^2 / ( variances * (n-1) ) )))
    
    
    # find critical t and results 
    # find the t-critics, hence the results
    
    mynewdata=as.list(values)
    names(mynewdata)=vars
    
    Y_pred=predict(mod, newdata=mynewdata) # non generalizzabile
    df=(n-2) # caso un predittore (Eq 2 Crawford & Garthwaite, 2006)
    
    t.obs= (obs - Y_pred)/Sn_1 # formula (3) by Crawford and Garthwaite
    
    if (upper==FALSE){
      p.obs=pt(t.obs, df = n-2) # taken from p.262 line 5.
    }
    
    if (upper==TRUE){
      p.obs=1-pt(t.obs, df = n-2)
    }    
  }
  
  # case: more than 1 predictor
  if (length(vars)>1){
    
    ## PREPARE SOME VARIABLES
    # get data.frame used in lm
    mod_data = mod$model
    
    # change order of columns for consistency
    # with vars
    mydata = (mod_data[, vars] )
    
    #get numerosity
    n = dim(mydata)[1]
    
    # get sigma
    sigma = summary(mod)$sigma
    
    # get number of variables
    nvar=length(vars)
    
    # calculate vars variances
    variances=apply(mydata[, vars], 2, var)
    # calvulcate vars means
    means=apply(mydata[, vars], 2, mean)
    
    
    
    ## CALCULATE VALUES
    
    rmat=cor(mydata[, vars]) # r mat is the correlation matrix
    rinv=solve(rmat) # r is the inverted correlation matrix (needed for the formula)
    
    rinv.diag=diag(rinv) # diag elements.
    rinv.offdiag=rinv[upper.tri(rinv)]
    
    zs=rep(NULL, length(vars))
    
    for (i in 1:length(vars)){
    zs[i]=((n-1)*(values[i]-means[i]))/sum((mydata[,i]-means[i])^2)
    }
    
    zio_2=zs^2
    
    
    A=rinv.diag%*%zio_2 # generalizable to any length of zio_2
    
    # to create B, first I create a matrix with all possible moltiplication between z
    # to do that I use cross-product.
    zio_2_multi=zio_2%o%zio_2
    
    # isolate all off-diagonal elements
    zio_2_multi.offdiag=zio_2_multi[upper.tri(zio_2_multi)]
    
    # calculate B
    B=rinv.offdiag%*%zio_2_multi.offdiag
    
    # Sn_1
    Sn_1=sigma*(sqrt( 1 + ( 1/n ) + ( 1/(n-1) ) * A + ( 2/(n-1) ) * B ))
    
    
    
    # find the t-critics, hence the results
    
    mynewdata=as.list(values)
    names(mynewdata)=vars
    
    
    
    Y_pred=predict(mod, newdata=mynewdata) # 
    
    t.obs= (obs - Y_pred)/Sn_1 # formula (3) by Crawford and Garthwaite
    
    
    if (upper==FALSE){
    p.obs=pt(t.obs, df = n-nvar-1) # taken from p.262 line 5.
    }
    
    if (upper==TRUE){
      p.obs=1-pt(t.obs, df = n-nvar-1)
    }
  }

return(p.obs)

}

