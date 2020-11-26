cluster_mass <-function(clusters, tmat){
  # IMPORTANT: the function assumes that clusters
  # is a electrodes*timepoints matrices (as the results of find.clusters)
  # and tmat is a timepoints*electrodes
  
  if(!is.null(clusters)){
    clusters.vec = unlist(t(clusters))
    tmat.vec = unlist(tmat)
    clust_mass.vec = tapply(tmat.vec, clusters.vec, sum)
    # remove cluster 0. No clusters is treated with 0 by the find.clusters function. 
    clust_mass.vec = clust_mass.vec[names(clust_mass.vec)!="0"]
  return(as.numeric(clust_mass.vec))
  } else {
    # if there are no clusters a 0 is returned (like no mass)
    # Not sure this is correct, but if NA is resturned, than no cluster found
    # during permutation is excluded
    return(0)
  }
}