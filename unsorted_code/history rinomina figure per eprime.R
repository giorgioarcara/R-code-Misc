setwd("~/Downloads/onlineimageresize_com_93a282dd_7e36_9f7d")

file.names=dir()

for (i in 1:length(file.names)){
	file.rename(file.names[i], gsub("onlineimageresize_com_", "", file.names[i]))
	
}