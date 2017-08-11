splitting.point<-function(base1=NULL, base2=NULL, numbers=NULL, startmsec=NULL, endmsec=NULL, erplist1=NULL, erplist2=erplist1, crit.npoints=10, crit.msec=NULL, electrodes="all", to.exclude=NULL, interval=c(startmsec, endmsec)) {


# preliminary checks
	if (is.null(erplist1)|is.null(erplist2)){
	stop("two erplist objects (erplist1 and erplist2) containing ERP data frames must be specified!", call.=F)
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

### to.exclude overwrite electrodes argument!
if (!is.null(to.exclude)){
	electrodes=names(erplist1[[paste(base1, numbers1[1], sep = "")]])[!names(erplist1[[paste(base1, numbers1[1], sep = "")]])%in%to.exclude]
	}

### select interval to be analyzed.
startpoint=round(msectopoints(interval[1], dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))
endpoint=round(msectopoints(interval[2], dim(erplist1[[paste(base1, numbers1[2], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec))


new.dim1=length(startpoint:endpoint)

# calculate exact interval analyzed
# the exact analyzed time window could differ from the one specified following approximations with msecotopoints.
# the exact values considered will be returned.
exact.interval.start=pointstomsec(startpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)
exact.interval.end=pointstomsec(endpoint, dim(erplist1[[paste(base1, numbers1[1], sep = "")]])[1], startmsec=startmsec, endmsec=endmsec)



# datall is a matrix with subject in columns and time x electrodes in rows.

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
    		datall1=c(datall1, x.vec)
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
    		datall2=c(datall2, x.vec)
        }
    }

## 
# STEP 2 


res.diff=datall1-datall2

# transform here. I checked by comparing with create diff and the results is the same.
# to check uncomment here
# res.diff

res.diff=as.numeric(res.diff>0)


# reconstruct a list of matrices (one for each subject)

res.diff.list=list(NULL)
length(res.diff.list)=length(numbers)
s.startpoint=1


for (i in 1:length(numbers)){

s.endpoint=(s.startpoint-1)+(dim(x.temp)[1]*dim(x.temp)[2])

res.diff.list[[i]]=matrix(res.diff[s.startpoint:s.endpoint], nrow=dim(x.temp)[1], ncol=dim(x.temp)[2], byrow=F)

s.startpoint=s.startpoint+(dim(x.temp)[1]*dim(x.temp)[2])

}




# cambio per creare un vettore collassando.
res.diff.list.str=list(NULL)
length(res.diff.list.str)=length(res.diff.list)

for (i in 1:length(res.diff.list.str)){
	res.diff.list.str[[i]]=apply(res.diff.list[[i]], 2, function(x){paste(x, collapse="")})
	}

library(stringr)

sampling.rate=function(x, baseline=NULL, total.length=NULL){
	samp.rate=((dim(x)[1]-1)*1000)/(-baseline+total.length)
	return(samp.rate)
}

mysamp.rate=sampling.rate(erplist1[[paste(base1, numbers1[1], sep = "")]], baseline=startmsec, total.length=endmsec)

if (!is.null(crit.msec)){
	crit.npoints=round(crit.msec/(1000/mysamp.rate))
	}
	
# calculate the exact ms, to be returned
exact.crit.msec=crit.npoints*(1000/mysamp.rate)


# DEFINE CRITERION
# look for n consecutive points
mycriterion=paste(rep(1, crit.npoints), collapse="")

# create a matrix for each subject

all.split.mat=matrix(NA, nrow=length(numbers), ncol=dim(x.temp)[2])


for (i in 1:length(numbers)) {

curr.subj=res.diff.list.str[[i]]
subj.split.mat=matrix(NA, nrow=1, ncol=length(curr.subj)) #creo un vettore riga. IL numero di colonne saranno sempre gli elettrodi.


for (j in 1:length(curr.subj)){
	curr.split=str_locate_all(curr.subj[j], paste(mycriterion, "+", sep=""))[[1]] #!!!!! NOTA IL + serve per il regexp e trova anche altro. Nota anche l'[[1]] è per accedere all'oggetto che viene creato in una lista.
	
	# check se dimensioni risultati sono maggiori di 0
	if (dim(curr.split)[1]>0){
			subj.split.mat[j]=pointstomsec(curr.split[1,1], dim(x.temp)[1], startmsec=interval[1], endmsec=interval[2])
		}
	}
	all.split.mat[i,]=subj.split.mat
	
}


all.split.dat=as.data.frame(all.split.mat)
names(all.split.dat)=names(x.temp)
all.split.dat$Subject=as.factor(numbers)


param=data.frame(crit.npoints=crit.npoints, exact.crit.msec=exact.crit.msec, interval.start=interval[1], interval.end=interval[2], exact.interval.start=exact.interval.start, exact.interval.end=exact.interval.end, analyzed.npoints=dim(x.temp)[1])

results=list(param=param, splitting.points=all.split.dat)
return(results)

}



