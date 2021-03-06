find.clusters<-function(tscores, thresh, chan_hood, thresh_sign){
  
  
  # https://github.com/dmgroppe/Mass_Univariate_ERP_Toolbox/blob/master/find_clusters.m
  
  
  #tscores=t(res$t.mat)
  
  #thresh=5
  
  #thresh_sign=-1 # metto un numero positivo
  
  
  
  # !!! Data ara arranged nchan*n_tpt
  
  
  n_chan=dim(tscores)[1] # get number of channels
  n_tpt=dim(tscores)[2] # get number of timepoints
  
  # !! creo una variabile Globale su MATLAB (non so se pu? creare problemi)
  clust_ids=NULL
 
  clust_ids=matrix(0, nrow=n_chan, ncol=n_tpt)
  
  time_matrix=t( replicate(n_chan, 1:n_tpt) ) # matrix 1:nchan * timepoints  (chans in riga)
  chan_matrix= replicate(n_tpt, 1:n_chan)  # crate matrix nchan * 1:timepoints (timepoints in colonna)
  
  if (thresh_sign > 0){
  
    #looking for positive clusters
    above_thresh_ids=which(tscores>=thresh);
  
    } else {
    
      #looking for negative clusters
      above_thresh_ids=which(tscores<=thresh);
    }
   
  above_thresh_times=time_matrix[above_thresh_ids];
  above_thresh_chans=chan_matrix[above_thresh_ids];
  
  # Clear chan & time matrix to save memory (just in case)
  rm(chan_matrix, time_matrix)
  
  n_above=length(above_thresh_ids);
  
  n_clust=0
  
  for (a in 1:n_above){
    
    voxel_id=above_thresh_ids[a];
    
    if(!clust_ids[voxel_id]){
    # this "if" goes if the "voxel" isn't in a cluster yet
        n_clust=n_clust+1;
      
        clust_ids[voxel_id]=n_clust;
        
        #go through all the remaining voxels and find all the above
        #threshold voxels this voxel is neighbors with and give them this
        #cluster # G: initially the not_checked voxels are all those above the thresh
        voxels_not_checked = rep(1, length(above_thresh_ids));
        
        check_me = rep(0, length(above_thresh_ids));
        
        check_me[a] = 1;
    } # close if !clust_ids
    
    while (sum(check_me>0)) {
      
      # find the indices of the non-zero elements and return only the first
      first = which(check_me>0)[1]
      new = follow_clust(n_above,first,n_clust,above_thresh_ids,above_thresh_times,above_thresh_chans,chan_hood,n_chan)
      check_me[new]=1
      voxels_not_checked[first]=0 # G: set to 0 the first voxel, now it is checked.
      check_me = check_me&voxels_not_checked # G: the voxel to be checked are those "not checked" & with "check me".
    
      } # close while
    
    } # close for( a in 1:n_above)
    
  clust_membership=clust_ids;

  return(clust_membership)
  
} # end function find_cluster