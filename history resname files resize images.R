####

# change working directory to directory with images
setwd("~/Downloads/onlineimageresize_com_93a282dd_2a5c_1fd3")

all.files=dir()

for (i in 1:length(all.files)){
	new.name=gsub("onlineimageresize_com_", "", all.files[i])
	file.rename(all.files[i], new.name)	
}
