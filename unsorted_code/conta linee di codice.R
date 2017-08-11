### CONTA LINEE DI CODICE IN VARI FILES

setwd("~/Desktop/R files/erpr/pkg/R")

files=dir()

tot.count=0

for (i in 1:length(files)){
	temp=readLines(files[i])
	temp2=gsub(" ", "", temp) #tolgo spazi, in modo da eliminare poi righe vuote e allineare il simbolo di commento # a sx.
	temp3=temp2[-grep("^#", temp2)]  # tolgo linee che cominciano con #
	temp4=temp3[!temp3==""] #tolgo righe solo con spazi
	tot.count=tot.count+length(temp4)
}