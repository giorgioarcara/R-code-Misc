# myfile è un file .csv esportato da bst con delle scout

#setwd("C:/Users/Mamma/Desktop/nuovi files/MEGHEM analisi marzo/MEGHEM analisi R")

#myfiles=c("scout_151119_1749.csv", "scout_151119_1750.csv")


bstscouts2erplist<-function(myfiles, scout.names=NULL, base.name="Exp1_subj", Subj.numbers=NULL){
  
  if (length(myfiles)!=length(scout.names)){
    stop("scout.names and myfiles must have the same length!")
  }
  
  if (length(base.name)>1){
    stop("this function works only for one condition at time.\n
         To import more than one condition use the function more than once.\n
        You can later  use c(), to concatenate the resulting erplist.")
  }
  
  
  mylist=list(NULL)
  length(mylist)=length(myfiles)
  
  for (i in 1:length(myfiles)){
    
    myfile=myfiles[i]

    mycon=readLines(myfile)
  
  
   
    
    
    Time0=read.table(textConnection(mycon[1]), sep=",")
    Time=Time0[,-length(Time0)] # in importing there could be a "NA" at the end
    
    ## retrieve Time
    if (!grep("Time", as.character(Time0[1,1]))){
      stop("The first line of the file is not the Time\n please be sure that in bst you elected export Comma separated with header .csv")
    }
  
    #retrieve startmsec and endmsec
    startmsec=Time[,2] # the first column is just the string "Time"
    endmsec=Time[, length(Time)]
    
    dat0=read.table(textConnection(mycon), sep=",")
    dat=dat0[-1 , -c(1, length(dat0))] # remove Time row (first) and the  "NA" col at the end
    
    
    mylist[[i]]=t(dat)
    
    
  }
  
  alldata=unlist(mylist)
  
  # create an array (timepoints * subjects * scout)
  array0=array(data=alldata, dim=c(dim(dat)[2], dim(dat)[1], length(myfiles)))
  
  if (!is.null(Subj.numbers)){
    if (dim(array0)[2]!=length(Subj.numbers)){
      stop(paste("Something went wrong:\n you specified ", length(Subj.numbers), " names in Subject.names, but the resulting number\n of subjects from the rearrangement is ", dim(array0)[2], sep=""))
      }
    } else {
      Subj.numbers=1:dim(array0)[2]
  }
 
  # I guessed the array dimensions and position by intuition, but they (by change) 
  # actually reshaped rightly the data.
  # I made some checks of correspondence array0 with mylist (TODO a check with brainstorm scout)
  
  # plot(mylist[[1]][,10]) # first scout, 10th subject
  # plot(array0[,10,1]) # first scout, 10th subject
  
  erplist.res=list(NULL)
  length(erplist.res)=dim(array0)[2] # subject numbers
  
  for (i in 1:length(erplist.res)){
  
  temp.dat=as.data.frame(array0[, i , ]) # all timepoints and scounts of i-th subjects
  names(temp.dat)=scout.names
  
  erplist.res[[i]]=temp.dat
  
  names(erplist.res)[[i]]=paste(base.name, Subj.numbers[i], sep="")
  
  }

  invisible(erplist.res)
  
}