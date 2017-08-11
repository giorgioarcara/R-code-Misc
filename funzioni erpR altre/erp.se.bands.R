erp.se.bands = function (grandaverage = NULL, erp.se = NULL, electrode = NULL){
  # function to create (for ggplot)
  # bands around the mean
  
  res = data.frame(lower = grandaverage[, electrode] - erp.se[, electrode], upper = grandaverage[, electrode] + erp.se[, electrode])
  
  # add electrode (with names)
  res[, electrode]= grandaverage[, electrode]
  
  return(res)
  
}