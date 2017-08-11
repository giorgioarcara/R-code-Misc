EEG.importfunction(type, interval)
type=1
interval="500-800"
import=read.table(paste("/Users/giorgioarcara/Desktop/ESPERIMENTO ERP 2 area report/export_type_",type,"-int_",interval,".dat", sep=""), skip=1,header=FALSE)
import2=import[,2:3]
amplitude=as.numeric(import2[,2])#colonna con le ampiezze
amplitude=matrix(amplitude, byrow=TRUE, nrow=23) #nota il by row=TRUE, altrimenti la matrice si dispone nella maniera scorretta!
dataset=as.data.frame(amplitude)
names(dataset)=import[1:34,2]
dat=rbind(dataset,dat)




