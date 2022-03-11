CGcut.off.multi_table<-function(controls_data=NULL, model = NULL, preds_list = NULL, poly_list=NULL,  Yobs = NULL, p.crit=0.05, upper=FALSE){
  
  # ver 0.31, 11/03/2022
  
  # IMPORTANT: 
  # to work the predictors in the lm model should be ordered such that polynomials terms are at the end, int he same order
  # of what is specified in the model#
  # e.g., age + education + age_2 + edu_2 is ok
  # e.g. age + age_2 + educaton + education_2 is not ok
  
  # TABLE OF MULTIVARIATE VERSION OF CG CUT-OFF
  
  # poly_list is a list specifying polinomials as a pair "name of variable and polynomial order" in the following form
  # list(c("Education", 2), c("CRI", 2)).
  
  # pred_list is a list including all variables.
  # e.g. list(Age=seq(20, 80, 10), Sex=c(0,1))
  
  if(any(is.na(controls_data))){
    controls_data=na.omit(controls_data)
    warning("The dataset included some NA. na.omit() has been applied to the data.", call.=F)
  }
  
  pred_grid = expand.grid(preds_list)
  
  results = pred_grid
  
  # case with no polynomial
  if (is.null(poly_list)){
    
    for(iComb in 1:dim(pred_grid)[1]){
      results$Score[iComb] = CGcut.off.multi(controls_data, model = model, preds = as.numeric(pred_grid[iComb,]), Yobs = NULL, p.crit=p.crit, upper=upper)$Y_obs_crit
    }
    
  }
  # case with polynomials
  if (!is.null(poly_list)){
    
    # first create the polynomials from the grid
    for (iPoly in 1:length(poly_list)){
      curr_name = poly_list[[iPoly]][1]
      curr_poly = poly_list[[iPoly]][2]
      curr_polyname = paste(curr_name, curr_poly, sep="_")
      # modify both results and pred_grid
      results[, curr_polyname] = results[, curr_name]^as.numeric(curr_poly)
      pred_grid[, curr_polyname]=pred_grid[, curr_name]^as.numeric(curr_poly)
      controls_data[, curr_polyname]=controls_data[, curr_name]^as.numeric(curr_poly)
    }
    
    for(iComb in 1:dim(pred_grid)[1]){
      results$Score[iComb] = CGcut.off.multi(controls_data, model = model, preds = as.numeric(pred_grid[iComb,]), Yobs = NULL, p.crit=p.crit, upper=upper)$Y_obs_crit
    } 
    
    
  }
  
  
  
  
  
  
  return(results) 
  
}
