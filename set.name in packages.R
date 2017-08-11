# questo script serve per un passo fondamentale per creare i pacchetti: quello di inserire un titolo alla funzione. Nello specifico come titolo mette semplicemente il nome della funzione.
## devi selezionare la directory "man" che contiene i files .Rd

directory=dir()

for (i in 1:length(directory)){
	Rdfile=readLines(directory[i])
	ini.name=grep("name\\{",Rdfile)
	name=Rdfile[ini.name]
	name=strsplit(name, "\\{")[[1]][2]
	name=gsub("\\}", "", name)

	ini.title=grep("title\\{",Rdfile)
	Rdfile[ini.title+1]=name #nel titolo è accapo.

	writeLines(Rdfile, directory[i])
	}