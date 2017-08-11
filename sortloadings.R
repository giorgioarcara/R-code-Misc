sortloadings <- function (x, PC=1){
  
  # get loadings
  l = x$rotation 
  
  # this functions returns the results of prcomp, sorting according the value of the PC specified, 
  lsort = l[ order( l[, PC], decreasing=T), ]
  
  return(lsort)
  
}