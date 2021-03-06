# you should launch this draft after "history_bozza_cluster_based_in_mass.t.test6_25_07_2018.R"

tic()
base1 = "m_qualche_pl_"
base2 = "m_qualche_sg_"
numbers1=Subject.numbers
numbers2=Subject.numbers
startmsec=my_startmsec
endmsec=my_endmsec 
erplist1=Exp2Second_bc
erplist2=Exp2Second_bc
paired=T
p.adjust.method="cluster.based.permutation"
to.exclude = c("VEOG_down", "HEOG_left", "HEOG_right", "Ear_right")
n.permutations=1000
stable.diff=FALSE
interval = c(0,600)
p.crit=0.05
na.rm=TRUE
min_nchans=3

# NOTE: this is mass.t.test version 6
# cluster based permutation implemented


# results include:
# - t.res.mat: the full matrix with T-values: NA are added in the timepoitns not analized.
# - or.res.filt.mat: the matrix, dysplaing only significant t-values
# - or.res.log.mat: a logical matrix. TRUE where p values were < p.crit (after corrections)
# - or.res.p.mat: matrix with observed p-values.

# all this matrices have the same timepoints as the ORIGINAL supplied matrices.

# preliminary checks
if (is.null(erplist1)|is.null(erplist2)){
  stop("two erplist objects (erplist1 and erplist2) containing ERP data frames must be specified!", call.=F)
}

# consistency checks for paired =T
if (paired==TRUE&(length(numbers1)!=length(numbers2))){
  stop ("if paired == TRUE, numbers1 and numbers2 must have equal length.", call.=F)
}

#### object checks
object.names1=paste(base1, numbers1, sep="")
if (any(!object.names1%in%names(erplist1))){
  missing.objects1=object.names1[!object.names1%in%names(erplist1)]
  missing.object.collist1=paste(missing.objects1, "\n", sep="")
  stop("The following objects are not contained in the erplist1 specified:\n", missing.object.collist1, call.=F)
}
#### object checks
object.names2=paste(base2, numbers2, sep="")
if (any(!object.names2%in%names(erplist2))){
  missing.objects2=object.names2[!object.names2%in%names(erplist2)]
  missing.object.collist2=paste(missing.objects2, "\n", sep="")
  stop("The following objects are not contained in the erplist2 specified:\n", missing.object.collist2, call.=F)
}



###
# STEP 1 LOAD DATA
#

# retrieve names from first subject.
if (electrodes[1]=="all"){
  electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])
}

### to.exclude overwrite electrodes!
if (!is.null(to.exclude)){
  electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])[!names(erplist1[[paste(base1, numbers1[1], sep = "")]])%in%to.exclude]
}

### select interval to be analyzed.
# determin star and endpoints
startpoint=round(msectopoints(interval[1], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
endpoint=round(msectopoints(interval[2], dim(erplist1[[paste(base1, numbers1[2], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))

# calculate exact interval analyzed
# the exact analyzed time window could differ from the one specified following approximations with msecotopoints.
# the exact values considered will be returned.
exact.interval.start=pointstomsec(startpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
exact.interval.end=pointstomsec(endpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)

###################
# CREATE DATALL ###
###################
# datall is a matrix with subjects in columns and time x electrodes in rows.
# if an interval is specified, datall already select the correct timepoints.

# store these info for future use
n_timepoints = length(startpoint:endpoint)
n_electrodes = length(electrodes)

# get original names for backup
or.dim1=dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1]
new.dim1=length(startpoint:endpoint)

or.dim2=dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[2]
or.electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])
new.dim2=length(electrodes)
  



datall1 = NULL
for (i in 1:length(numbers1)) {
  x.temp = erplist1[[paste(base1, numbers1[i], sep = "")]][startpoint:endpoint, electrodes] #note that I select the electrodes
  
  # add exception if only one electrode is included
  if(is.null(dim(x.temp)[1])){
    x.temp=data.frame(x.temp)
    names(x.temp)=electrodes
  }
  
  x.vec=unlist(x.temp)
  if (i == 1) {
    datall1 = x.vec
  }
  if (i != 1) {
    datall1=cbind(datall1, x.vec)
  }
}

datall2 = NULL
for (i in 1:length(numbers2)) {
  x.temp = erplist2[[paste(base2, numbers2[i], sep = "")]][startpoint:endpoint, electrodes] #note that I select the electrodes
  
  # add exception if only one electrode is included
  if(is.null(dim(x.temp)[1])){
    x.temp=data.frame(x.temp)
    names(x.temp)=electrodes
  }
  
  
  x.vec=unlist(x.temp)
  if (i == 1) {
    datall2 = x.vec
  }
  if (i != 1) {
    datall2=cbind(datall2, x.vec)
  }
}

#########################################
# T.TESTS POINT by POINT (calculate, by defaults, all t-tests)
#########################################
# this t.tests are calcualted in a way to optimze the speed
# (I don't rely on t.test() function but calculate manually the results)

# initialize some objects
allres=rep(NA, dim(datall1)[1]) # uso datall1 come riferimento ma la dimensione (i timepoints) saranno uguale a datall2
allres.t=rep(NA, dim(datall1)[1]) # creo matrice per valori di t


### OBSERVED T-TESTS: BETWEEN
if (paired ==  FALSE) {
  
  
  for (i in 1:dim(datall1)[1]){
    
    # res=t.test(datall1[i,], datall2[i, ], var.equal=T, paired=F)$p.value
    
    df=length(datall1[i, ]) -1 + length(datall2[i, ]) -1  # remember that cols of datall are the subjects.
    t.crit=qt(p.crit/2, df=df) # non lo uso per ora. Non sono manco sicuro sia giusto
    
    res.t=(mean(datall1[i, ], na.rm=T)-mean(datall2[i, ], na.rm=T))/sqrt(var(datall1[i,], na.rm=T)/length(datall1[i,])+var(datall2[i, ], na.rm=T)/length(datall2[i,])) # independent samples.
    
    res=pt(abs(res.t), df=df, lower.tail=F)*2
    
    allres[i]=res #p-value
    
    allres.t[i]=res.t
    
  }
}

### OBSERVED T-TESTS: PAIRED
if (paired == TRUE) {
  
  #df=length(datall1[i, ]) -1 
  
  for (i in 1:dim(datall1)[1]){
    
    df= sum(!is.na(datall1[i, ])) - 1 
    t.crit=qt(p.crit/2, df=df) # non lo uso per ora.
    
    res.t=mean((datall1[i, ]-datall2[i, ]), na.rm = na.rm)/sqrt((var(datall1[i, ]-datall2[i, ], na.rm=na.rm)/length(datall1[i, ])))
    # dependent samples
    
    res=pt(abs(res.t), df=df, lower.tail=F)*2
    
    
    allres[i]=res
    
    
    allres.t[i]=res.t
    
  }
}

########################################
#### APPLY P.ADJUST CORRECTIONS ########
#######################################
# apply corrections already implemented in 
# p.adjust.methods function (so in base R)

if (!p.adjust.method%in%c("tmax", "cluster.based.permutation")){
  
  allres=p.adjust(allres, method=p.adjust.method)
}


########################################
#### tMAX permutations ########
#######################################

# removed for clearer code


##############################
# reconstruct the matrix to return the reuslts
##############################
# the following part is the same for simple t.tests and tmax permutation
# I write it here to avoid replicating code.
# the results may also be fed to the stable.diff fun.

# INPUT: allres is a vector with all tests
# OUTPUT: 
# - res.mat, res.p.mat, etc. are matrices with n_timpoints * n_electrodes
# - or.res.mat, or.res.p.mat, etc. are matrices with the original dimension of fed data (NA in timepoints and electrodes not used)

# NOTE: that the following code cannot be used for cluster based permutation
# in which negative and positive results are filtered separately



## filtro i risultati  di res.t.mat a partire da crit.mat e riarrangio al volo.
res.mat=matrix(allres, nrow=n_timepoints, ncol=n_electrodes, byrow=F)
res.t.mat=matrix(allres.t, nrow=n_timepoints, ncol=n_electrodes, byrow=F)
res.p.mat=matrix(allres, nrow=n_timepoints, ncol=n_electrodes,  byrow=F)
colnames(res.t.mat)=electrodes


#### 
res.mat.filter=matrix(NA, nrow=dim(res.mat)[1], ncol=dim(res.mat)[2])
res.log.mat=res.mat<p.crit
res.mat.filter[res.log.mat]=TRUE

filt.mat=matrix(res.t.mat[res.mat.filter], nrow=nrow(res.mat), ncol=ncol(res.mat))

## reconstruct a data.frame with the original number of time points before returning results
# "or" prefix indicates that the dimensions are referred to the original data dimensions
or.filt.mat=as.data.frame(matrix(rep(NA, or.dim1*dim(x.temp)[2]), nrow=or.dim1, ncol=dim(x.temp)[2]))

# IMPORTANT! here the matrix is compsed by FALSE or TRUE (no NA) to be used properly with stable.diff
or.res.log.mat=as.data.frame(matrix(rep(FALSE, or.dim1*dim(x.temp)[2]), nrow=or.dim1, ncol=dim(x.temp)[2])) # creo lo stesso per risultati log
or.res.t.mat=or.filt.mat # the same for t.
or.res.p.mat=or.filt.mat # and for p

or.filt.mat[startpoint:endpoint, ]=filt.mat
or.res.log.mat[startpoint:endpoint, ]=res.log.mat
or.res.t.mat[startpoint:endpoint, ]=res.t.mat
or.res.p.mat[startpoint:endpoint, ]=res.p.mat


colnames(or.filt.mat)=names(x.temp)
colnames(or.res.log.mat)=names(x.temp)
colnames(or.res.t.mat)=names(x.temp)
colnames(or.res.p.mat)=names(x.temp)

#########################################
####### CALCULATE EXACT MS OF INTERVAL
#########################################
startpoint=round(msectopoints(interval[1], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
endpoint=round(msectopoints(interval[2], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))

or.dim1=dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1]
new.dim1=length(startpoint:endpoint)

# calculate exact interval analyzed
# the exact analyzed time window could differ from the one specified following approximations with msecotopoints.
# the exact values considered will be returned.
exact.interval.start=pointstomsec(startpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
exact.interval.end=pointstomsec(endpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)

############################################
# STABLE DIFF (applied to regular t-tests and t-max)
############################################
if (stable.diff==TRUE){ 
  
  ####### CALCULATE CRIT.MSEC (for stable.diff)
  
  mysamp.rate=sampling.rate(erplist1[[paste(base1, numbers1[1], sep = "")]], startmsec=startmsec, endmsec=endmsec)
  
  if (!is.null(crit.msec)){
    crit.npoints=round(crit.msec/(1000/mysamp.rate))
  }
  
  # calculate the exact ms, to be returned in param
  exact.crit.msec=crit.npoints*(1000/mysamp.rate)
  
  stable.diff.res=stable.diff.fun(or.res.log.mat, electrodes, crit.npoints, interval, startmsec, endmsec)
  
  # "filter" again the data, on the basis of stable.diff.fun results
  
  #print(str(stable.diff.res, 1))
  or.filt.mat[!stable.diff.res$res.log.mat]=NA
  or.res.log.mat[!stable.diff.res$res.log.mat]=NA
  or.res.t.mat[!stable.diff.res$res.log.mat]=NA
  or.res.p.mat[!stable.diff.res$res.log.mat]=NA
  
} # end if (stable.diff==TRUE)

#######################################
#### CLUSTER BASED PERMUTATION
#######################################

# NOTE GIORGIO (remove here to simplify debug) 
#  if (p.adjust.method=="cluster.based.permutation"){

# initilaize neighbours
neighbours = NULL

## check if it possible to perform the cluster based the cluster based permutation


### WITH THIS FUNCTION I MUST RETURN
# - significant results filtered  (perm.or.filt.result)
# - a logical matrix with significance: check the format in mass.t. here 
#        temp.res.log.mat is maybe NOT ok, cause it has NA inside. Check
# - cluster membership
# - cluster exact p.values


# preliminary check if there is any significant result (otherwise the functions called
# for the cluster based permutation will give errors.)
if(all(is.na(or.filt.mat))){
  
  stop("there are no significant results and cluster based permutation cannot be performed")
  
}



# steps of cluster based permutation.
# taken from Groppe, Urbach, & Kutas (2011). Mass univeriate analysis of event-related brain potential/fields 1: A critical tutorial review.

# step 1) calculate t.scores.  
# step 2) exclude p not exceding a threshold (e.g. p>= 0.05)
# step 3a) determine neighbours (added by G)
# step 3) optional (use only t scores with at least some significant neighbours)
# step 4) on remaining t.scores, form cluster (the permutation start here!)
# step 5) sum values of each cluster and find cluster.level.t. this is the cluster level t.score
# step 6) On each permutation get the more extreme values to define the null hypothesis ditsribution(as in tmax) (the permutation end here!)
# step 7) p.value of each cluster is derived from its ranking on the null hypothesis distribution
# step 8) each member of the cluster is assigned the p-value of the entire cluster.


# step 5) A) determine neighbours if not supplied
# if you don't specify
if(is.null(neighbours)){
  
  ## retrieve electrode coordinates data.frame from topoplot
  elec.coord=topoplot(return.coord=TRUE)
  
  # electrode names can be different simply for having upper-case or lower-case letters
  # tackle this issue by making all electrodes upper-case
  
  # transform current electrode names toupper to match them with the electrode list from topoplot
  up.curr.el = toupper(electrodes)
  # transform all electrode names in list to upper
  up.elec.coord.names = toupper(elec.coord$el.name)
  
  # retrieve current electrode coordinates
  curr.elec.coord = elec.coord[up.elec.coord.names %in% up.curr.el, ]
  
  # return a matrix n_electrodes x n_electrodes with 0/1 indicating neigbourhood
  neighbours=spatial_neighbours(curr.elec.coord[, c("x", "y", "z")], 1)
}

## step 5) FIND CLUSTERS

# the find cluster from Groppe uses a threshold (positive or negative).
# you have already calculated this critical t in the first mass univariate
# the object is called "t.crit" (see inside the mass t.test calculations) 

# NOTE: you need to calculate two times the cluster, one for positive, and one for negative
# cluster. THis is because clusters are calculated according to sign.
# (Note that I transform the threshold to positive to be sure to know its sign, positive or negative)
# in the call to find.cluster


# first I calcualte the actual clusters of my data, to be compared with the empirical distributions
# of cluster masses.

perm.thresh=abs(t.crit)

## the function find.cluster does not work with NA.
# filt.mat (the data I need to work with, is full of NA)
# hence I substitute NA with 0 (that I know for sure they won't be taken as significant value)

# store a new object called "obs.perm.mat", to keep the original "filt.mat" object
obs.perm.filt.mat=filt.mat

obs.perm.filt.mat[is.na(obs.perm.filt.mat)]=0

#######################################
# 1) CALCULATE ACTUAL CLUSTERS
########################################


# find positive clusters
obs.posclust=find.clusters(t(obs.perm.filt.mat), thresh = perm.thresh, chan_hood = neighbours, thresh_sign = 1, min_nchans=min_nchans)
# find negative clusters
obs.negclust=find.clusters(t(obs.perm.filt.mat), thresh = -perm.thresh, chan_hood = neighbours, thresh_sign = -1, min_nchans=min_nchans)

# NOTE!! the current find.clusters function return either a 
# electrodes*timepoints matrix OR just the NULL.
# you should modify the function to avoid exceptions in the following code.

##########################################
## Calculate MASS of CLUSTERS
###########################################  
# the mass of a cluster is the sum of t-values of all members of a cluster


##########################################
## Calculate mass of positive clusters 
###########################################
mass.obs.posclust=cluster_mass(obs.posclust, obs.perm.filt.mat)
##########################################
## Calculate mass of negative clusters 
###########################################
mass.obs.negclust=cluster_mass(obs.negclust, obs.perm.filt.mat)



# permutations are made from datall1 and datall2

if (paired == FALSE){
  
  # create a unique data.frame with all subjects
  # the aim is to permute every subject swiching subject
  # group in the permutations
  # notice that this is not necessary with paired=T
  # in that case I simply permute the signs of the 
  # condition differences
  
  
  datall12=cbind(datall1, datall2)
  # creo vettore con numero totale di soggetti
  all.numbers=1:dim(datall12)[2] 
  
  
  # create object with all permutations results
  # the function ha two rows: the first for 
  # positive cluster max results and the second
  # for negative cluster max results.
  # This is because the p-values of positive and negative clusters 
  # are calculated separately
  # 
  clust.perm.max.res=matrix(0, nrow=2, ncol=n.permutations)
  
  
  # set length outside the for loop to speed up computation.
  # I will need them inside the loop for i in 1:n.permutations
  datall1.perm.len=rep(dim(datall1)[2], dim(datall1)[1])
  datall2.perm.len=rep(dim(datall2)[2], dim(datall2)[1])
  
  
  # set timer for permutations:
  n.points.time=floor(seq(1, n.permutations, n.permutations/10))
  time.elapsed=0
  
  
  
  #### START PERMUTATIONS HERE
  cat("computing permutations:\n")
  
  for (i in 1:n.permutations){
    
    perm.index1=sample(all.numbers, length(numbers1))
    datall1.perm=datall12[,perm.index1]
    datall2.perm=datall12[,-perm.index1]
    
    curr.perm.t=rep(NA, dim(datall1.perm)[1])
    
    
    datall1.perm.mean=rowMeans(datall1.perm)
    datall2.perm.mean=rowMeans(datall2.perm)
    datall1.perm.var=rowSums((datall1.perm-datall1.perm.mean)^2)/(datall1.perm.len)
    datall2.perm.var=rowSums((datall2.perm-datall2.perm.mean)^2)/(datall2.perm.len)
    
    
    curr.perm.t=(datall1.perm.mean-datall2.perm.mean)/sqrt(datall1.perm.var/datall1.perm.len+datall2.perm.var/datall2.perm.len)
    
    ## reconstruct the data, to use the various cluster perm functions (which require matrices)
    curr.perm.t.mat=matrix(curr.perm.t, nrow=dim(obs.perm.filt.mat)[1], ncol=dim(  obs.perm.filt.mat)[2], byrow=F)
    colnames(curr.perm.t.mat)=colnames(obs.perm.filt.mat)
    
    #######################################
    # 1) CALCULATE CURRENT PERMUTATION CLUSTERS
    ########################################
    
    # NOTICE that curr.perm.t.mat is filtered both for signficance
    
    
    # find positive clusters
    curr.posclust=find.clusters(t(curr.perm.t.mat), thresh = perm.thresh, chan_hood = neighbours, thresh_sign = 1, min_nchans=min_nchans)
    # find negative clusters
    curr.negclust=find.clusters(t(curr.perm.t.mat), thresh = -perm.thresh, chan_hood = neighbours, thresh_sign = -1, min_nchans=min_nchans)
    
    ##########################################
    ## Calculate MASS of CLUSTERS (within perm)
    ###########################################  
    
    
    ##########################################
    ## Calculate mass of positive clusters 
    ###########################################
    mass.curr.posclust=cluster_mass(curr.posclust, curr.perm.t.mat)
    ##########################################
    ## Calculate mass of negative clusters 
    ###########################################
    mass.curr.negclust=cluster_mass(curr.negclust, curr.perm.t.mat)
    
    
    # 3) then store the maximum mass cluster value in the object clust.perm.max.res
    # note that I first concatenate all values, then transform in absolute value, and
    # then I find the max.
    
    ## ERRORE GIORGIO
    # https://github.com/dmgroppe/Mass_Univariate_ERP_Toolbox/blob/master/clust_perm2.m
    # qui è segnato che i p-value di negative cluster e positive devono essere diversi.
    # -When doing two-tailed tests it is possible to get p-values greater than 1.
    # These can be treated equivalent to a p-value of 1. The reason for
    # this is that the p-values for positive and negative clusters are computed
    # from two different distributions of permuted values.  For a non-cluster
    # based permutation test, there is only one distribution of permuted
    # values.
    
    
    ## store the results of max and min cluster separately
    # (NOTA GIORGIO: controlla il codice nella funzione di Groppe per capire esattamente questa parte.
    clust.perm.max.res[1, i]=max(c(mass.curr.posclust))
    clust.perm.max.res[2, i]=min(c(mass.curr.negclust))
    
    
    if (i%in%n.points.time){
      cat(rep(".",10-time.elapsed), "\n")
      time.elapsed=time.elapsed+1
      
    }
    
  } # end for i in 1:permutations in clust based perm and paired = F
  
  
} # end if method = clust based perm & paired =F	


# GIORGIO UNCOMMENT
if (paired == TRUE) { # cluster.based.permutation paired case
  
  
  # datall12 is the dataframe with the difference of datall1 and datall2.
  datall12=datall1-datall2
  
  # create object with all permutations results
  # the function ha two rows: the first for 
  # positive cluster max results and the second
  # for negative cluster max results.
  # This is because the p-values of positive and negative clusters 
  # are calculated separately
  # 
  clust.perm.max.res=matrix(0, nrow=2, ncol=n.permutations)
  
  # create object with length to speed-up computation
  # the object nsubjects * (points*nelectrodes)
  datall.perm.len=rep(dim(datall1)[2], dim(datall1)[1])
  
  
  # set timer for permutations:
  n.points.time=floor(seq(1, n.permutations, n.permutations/10))
  time.elapsed=0
  cat("computing permutations:\n")
  
  # GIORGIO UNCOMMENT
  
  for (i in 1:n.permutations){
    
    # assign -1 and 1 to switch labels for the permutations
    perm=sample(c(1,-1), length(numbers1), replace=T)
    
    # multiply the matrix with observed t.
    # the effect is to "switch" labels of the condition (since datall12 is already the difference A-B)
    datall.perm=t(t(datall12)*perm)
    
    datall.perm.mean=rowMeans(datall.perm)
    datall.perm.var=rowSums((datall.perm-datall.perm.mean)^2)/(datall.perm.len)
    
    curr.perm.t=datall.perm.mean/sqrt(datall.perm.var/datall.perm.len)
    
    # reconstruct the data, in order to use find.clusters and stable.diff.fun
    curr.perm.t.mat=matrix(curr.perm.t, nrow=dim(obs.perm.filt.mat)[1], ncol=dim(obs.perm.filt.mat)[2], byrow=F)
    colnames(curr.perm.t.mat)=colnames(obs.perm.filt.mat)
    
    
    #######################################
    # 1) CALCULATE CURRENT PERMUTATION CLUSTERS
    ########################################
    
    # NOTICE that curr.perm.t.mat is filtered both for signficance
    
    
    # find positive clusters
    curr.posclust=find.clusters(t(curr.perm.t.mat), thresh = perm.thresh, chan_hood = neighbours, thresh_sign = 1, min_nchans=min_nchans)
    # find negative clusters
    curr.negclust=find.clusters(t(curr.perm.t.mat), thresh = -perm.thresh, chan_hood = neighbours, thresh_sign = -1, min_nchans=min_nchans)
    
    ##########################################
    ## Calculate MASS of CLUSTERS
    ###########################################  
    
    ##########################################
    ## Calculate mass of positive clusters 
    ###########################################
    mass.curr.posclust=cluster_mass(curr.posclust, curr.perm.t.mat)
    ##########################################
    ## Calculate mass of negative clusters 
    ###########################################
    mass.curr.negclust=cluster_mass(curr.negclust, curr.perm.t.mat)
    
    
    # https://github.com/dmgroppe/Mass_Univariate_ERP_Toolbox/blob/master/clust_perm2.m
    # -When doing two-tailed tests it is possible to get p-values greater than 1.
    # These can be treated equivalent to a p-value of 1. The reason for
    # this is that the p-values for positive and negative clusters are computed
    # from two different distributions of permuted values.  For a non-cluster
    # based permutation test, there is only one distribution of permuted
    # values.
    
    ## store the results of max and min cluster separately
    clust.perm.max.res[1, i]=max(mass.curr.posclust)
    clust.perm.max.res[2, i]=min(mass.curr.negclust)
    
    # IMPORTANT: substitute NA or Inf NOTE: TO DECIDE
    # this is otherwise there are odd results I don't know where they come
    # I suppose from a /0
    # and NA (so no cluster found)
    #clust.perm.max.res[clust.perm.max.res%in%c(Inf, -Inf)]=0
    #clust.perm.max.res[is.na(clust.perm.max.res)]=0
    
    if (i%in%n.points.time){
      cat(rep(".",10-time.elapsed), "\n")
      time.elapsed=time.elapsed+1
    }	
  } # end  for loop (i in 1:n.permutations)
  # GIORGIO UNCOMMENT
} # end clust based perm and if paired == TRUE

##############################
## PREPARE RESULTS PERMUTATION
##############################
# results are returned in this way:
# - res.tmat = the observed tmat
# - mass.neg.results = an array with timepoints * electrodes * n_sign clusters


##
# - first calculate the actual p value associated to each mass
# - assign that p-values to all members of the cluster

#### YOU NEED TO THINK ABOUT THE BEST OUTPUT FOR CLUSTER BASED PERM: this is the real issue.


###########################
#### NEGATIVE CLUSTERS  ###
############################
# create a mask of results to be applied to the matrix and the filt.mat
# start from observed results
if (!is.null(obs.negclust)){
  
  # transpose negative clusters: as results from find.results they are electrodes*timepoins insetaedo
  # of timepoints * electrodes
  obs.negclust = t(obs.negclust)
  
  # initialize object for the results
  mass.neg.results = array(NA, dim=c(n_timepoints, length(electrodes), length(mass.obs.negclust)))
  
  # calculate all p-values of mass of negative clusters
  # calculate p-values as sum of times that the permuted values are smaller or equal than the observed value.
  # put in other terms, you calculate how unlikely is to observe your observed results.
  neg.clust.p = sapply(mass.obs.negclust, function(x){sum(clust.perm.max.res[2,]<=x, na.rm=T)/length(clust.perm.max.res[2,])})
  # retrieve cluster names (the number corresponding to cluster) 
  neg.clustnames = unique(as.numeric(obs.negclust)) # get unique values
  neg.clustnames = neg.clustnames[neg.clustnames!=0] # exclude 0 which means no cluster
  
  for (iClust in 1:length(neg.clustnames)){

    # start from unfiltered data
    curr.clust.filt.mat =  res.t.mat
    # filter with cluster belonging
    curr.clust.filt.mat[obs.posclust!=neg.clustnames[iClust]]=NA
    # filter with p-values # NOTE GIORGIO: to be removed, to allow online filtering of p-values
    curr.clust.filt.mat[neg.clust.p[iClust]>=p.crit]=NA
    # update the general object
    mass.neg.results[,,iClust] = curr.clust.filt.mat
     
  }
  
} else {
  # this NA objects are created if there were NO significant values already in the observed data
  # matrix timepoinsts * electrode with NA
  neg.clust.p = NA
  mass.neg.results = matrix(NA, nrow=n_timepoints, ncol=length(electrodes))
}

###########################
#### POSITIVE CLUSTERS  ###
############################
if (!is.null(obs.posclust)){
  
  # transpose posative clusters: as results from find.results they are electrodes*timepoins insetaedo
  # of timepoints * electrodes
  obs.posclust = t(obs.posclust)
  
  # initialize object for the results
  mass.pos.results = array(NA, dim=c(n_timepoints, length(electrodes), length(mass.obs.posclust)))
  
  # calculate all p-values of mass of posative clusters
  # calculate p-values as sum of times that the permuted values are smaller or equal than the observed value.
  # put in other terms, you calculate how unlikely is to observe your observed results.
  pos.clust.p = sapply(mass.obs.posclust, function(x){sum(clust.perm.max.res[2,]>=x, na.rm=T)/length(clust.perm.max.res[2,])})
  # retrieve cluster names (the number corresponding to cluster) 
  pos.clustnames = unique(as.numeric(obs.posclust)) # get unique values
  pos.clustnames = pos.clustnames[pos.clustnames!=0] # exclude 0 which means no cluster
  
  for (iClust in 1:length(pos.clustnames)){
    
    # start from unfiltered data
    curr.clust.filt.mat =  res.t.mat
    # filter with cluster belonging
    curr.clust.filt.mat[obs.posclust!=pos.clustnames[iClust]]=NA
    # remove not significant results
    curr.clust.filt.mat[pos.clust.p[iClust]>=p.crit]=NA
    
    # update the general object
    mass.pos.results[,,iClust] = curr.clust.filt.mat

  }
  
} else {
  # this NA objects are created if there were NO significant values already in the observed data
  # matrix timepoinsts * electrode with NA
  pos.clust.p = NA
  mass.pos.results = matrix(NA, nrow=n_timepoints, ncol=length(electrodes))
}


##################################################
# INITIALIZE FINAL OBJECTS FOR RESULTS
##################################################

#########################################################
# PREPARE RESULTS WITH THE ORIGINAL SIZE
#########################################################
# in this section I start from the object already calculated and I return matrices with the same dimension
# of the original supplied matrices. 
# This is very useful for consistency and to keep the same dimensions across analysis.
# in the results, the actual timepoints/points used are returned


#######
# mass.negitive.results with original dimensions
mass.or.neg.results=array(NA, dim=c(or.dim1, or.dim2, length(neg.clust.p))) # note that if NO clusters. length will be 1 cause I created a dummy object
colnames(mass.or.neg.results)=or.electrodes
# fill this array with right dimensions:
for (iClust in 1:length(neg.clust.p)){
  mass.or.neg.results[startpoint:endpoint,electrodes,iClust] = mass.neg.results[,,iClust]
}

#######
# mass.positive.results with original dimensions
mass.or.pos.results=array(NA, dim=c(or.dim1, or.dim2, length(neg.clust.p))) # note that if NO clusters. length will be 1 cause I created a dummy object
colnames(mass.or.pos.results)=or.electrodes
# fill this array with right dimensions:
for (iClust in 1:length(neg.clust.p)){
  mass.or.pos.results[startpoint:endpoint,electrodes,iClust] = mass.pos.results[,,iClust]
}

# return cluster belonging in original dimension
# positive clusters
or.obs.posclust=array(NA, dim=c(or.dim1, or.dim2))
colnames(or.obs.posclust)=or.electrodes
or.obs.posclust[startpoint:endpoint, electrodes]=obs.posclust

# negative clusters
or.obs.negclust=array(NA, dim=c(or.dim1, or.dim2))
colnames(or.obs.negclust)=or.electrodes
or.obs.negclust[startpoint:endpoint, electrodes]=obs.negclust




#  }# end if method == cluster.based.permutations
# fine clust.based permutation

### cambio i risultati NA in 0 ###

#or.filt.mat[is.na(or.filt.mat)]=0
# questo mi serve temporaneamente per usare mass.t

# all cases excluded cluster based (which requires a different structure)
if (p.adjust.method!="cluster.based.permutation"){
  
  param=data.frame( interval.start=interval[1], interval.end=interval[2], exact.interval.start=exact.interval.start, exact.interval.end=exact.interval.end, analyzed.npoints=dim(x.temp)[1], total.n.test=length(allres), p.correction=p.adjust.method)
  
  allresults=list(param=param, mass.t.results=or.filt.mat, sig=or.res.log.mat, t.mat=or.res.t.mat, p.mat=or.res.p.mat)
  
  if (stable.diff==TRUE){
    param=cbind(param, crit.msec=crit.msec, exact.crit.msec=exact.crit.msec)
    allresults=c(allresults, crit.list=list(stable.diff.res$crit.list))
  }
}

# case cluster based premutation
if (p.adjust.method=="cluster.based.permutation"){
  param=data.frame( interval.start=interval[1], interval.end=interval[2], exact.interval.start=exact.interval.start, exact.interval.end=exact.interval.end, analyzed.npoints=dim(x.temp)[1], total.n.test=length(allres), p.correction=p.adjust.method)
  allresults=list(param=param, mass.t.results=or.filt.mat, clust.pos.results=mass.or.pos.results, clust.neg.result=mass.or.neg.results, 
                  sig.pos= pos.clust.p, # calculate here on the fly
                  sig.neg = neg.clust.p, 
                  pos.clusters=or.obs.posclust, neg.clusters=or.obs.negclust)
  
}


## TO BE DONE:
# - check if clusters are computed correctly (some odd results). You can debug with brainstorm!
# - check how to deal with very short results

#invisible(allresults)

#}
toc()


