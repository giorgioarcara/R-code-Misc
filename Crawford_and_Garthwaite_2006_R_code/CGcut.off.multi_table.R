CGcut.off.multi_table<-function(controls_data=NULL, model = NULL, preds_list = NULL,  Yobs = NULL, p.crit=0.05, upper=FALSE){

  # ver 0.2, 21/08/2020
  
  # TABLE OF MULTIVARIATE VERSION OF CG CUT-OFF
  
# pred_list is a list including all variables.
# e.g. list(Age=seq(20, 80, 10), Sex=c(0,1))
  
  pred_grid = expand.grid(preds_list)
  
  results = pred_grid
  
  
  for(iComb in 1:dim(pred_grid)[1]){
    results$Score[iComb] = CGcut.off.multi(controls_data, model = model, preds = as.numeric(pred_grid[iComb,]), Yobs = NULL, p.crit=p.crit, upper=upper)$Y_obs_crit
  }
  
  
 return(results) 
  
  
}
  