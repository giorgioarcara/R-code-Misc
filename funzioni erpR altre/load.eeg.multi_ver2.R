# in questa funzione non uso più base e numbers, ma obj. Non l'ho più sviluppata perché creava casino di consistenza tra soggetti.

load.eeg.multi <-
function(files, outname=NULL, env=.GlobalEnv){
	for (i in 1:length(files)){
		eegout=read.table(files[i], header=T,skip=1)
		eeg.subjectname=readLines(files[i], n=1)
		eeg.subjectname=gsub("\t","", eeg.subjectname)
		comment(eegout)=eeg.subjectname
		if (is.null(outname))
		{
			eegout.name=gsub(".txt$", "", files[i])
			assign(eegout.name, eegout, envir=env)
		}
		if (!is.null(outname))
		{	
		eegout.name=paste(outname, i, sep="")
		assign(eegout.name, eegout, envir=env)
		}
	}
}
