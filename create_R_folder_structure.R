################################
# SCRIPT CREATE FOLDER STRUCTURE
################################
# this script creates the folder structure I use for R analysis.
# just include tehe initial path and the whole structure will be created.
# 24/06/2017

# initial dir 
ini_dir = "/Users/giorgioarcara/Documents/APACS/APACS - Neuro/APACS Neuro Comitato Etico/APACS - Parkinson/"



# create Figures
dir.create(paste(ini_dir,"/Figures", sep="" ))
dir.create(paste(ini_dir,"/Original Data", sep="" ))
dir.create(paste(ini_dir,"/R Data", sep="" ))
dir.create(paste(ini_dir,"/R functions", sep="" ))
dir.create(paste(ini_dir,"/R scripts", sep="" ))
dir.create(paste(ini_dir,"/Results files", sep="" ))
