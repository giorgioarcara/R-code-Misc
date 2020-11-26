follow_clust<-function(n_above,current_voxel_id,current_clust_num,above_thresh_ids,above_thresh_times,above_thresh_chans,chan_hood,n_chan){
  
  # NOTE: 25/07/2018 added n_chan, because returned errors
  # a = 5
  
  new_members=matrix(0, nrow=1, ncol=n_chan*3) 
  new_members_ct=0
  
  #
  #assign("clust_ids", clust_ids, envir=globalenv())
  
  for (b in current_clust_num : n_above){
    
    if (!clust_ids[above_thresh_ids[b]]){
      
      temp_dist=abs(above_thresh_times[b]-above_thresh_times[current_voxel_id]);
      
      if (above_thresh_chans[current_voxel_id]==above_thresh_chans[b]){
        #voxels are at same channel
        chan_dist=0;
        
      } else if (chan_hood[above_thresh_chans[current_voxel_id] , above_thresh_chans[b]]) {
        #channels are neighbors
        chan_dist=1;
      } else {
        #voxels aren't spatially compatible
        chan_dist=2
      }
      
      if ((temp_dist+chan_dist)<=1) {
        #if voxels are at same time point and neighboring channels OR
        #if voxels are at same channel and neighboring time points,
        #merge them into the same cluster
        
        clust_ids[above_thresh_ids[b]]<<-current_clust_num;
        #keep track of which other voxels are joined to this
        #cluster
        new_members_ct=new_members_ct+1;
        new_members[new_members_ct]=b;
      } # if ((temp_dist)
    }# if !clust
  } # for (b)
  
  return(new_members)
} # end function follow_clust