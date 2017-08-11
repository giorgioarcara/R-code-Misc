#questa funzione si interfaccia con la funzione exportpicture_02_h, che esporta fil .eeg che contengono eeg.

read.eeg=function(filename){
	eegfile=readLines(filename)
	end_header=grep("end_header", eegfile)
	eegout=read.table(filename,header=TRUE, skip=end_header)
	header=readLines(filename, n=end_header-1)
	header=gsub("\t","  ",header) #sostituisco i \t in spazi.
	comment(eegout)=header
	return(eegout)
	}